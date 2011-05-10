{-# LANGUAGE RankNTypes,ScopedTypeVariables,NamedFieldPuns,RecordWildCards,DeriveDataTypeable #-}
{-

Loading 10MB of 20000 copies of random TestAllTypes shows way too much garbage collection pressure.

Try and improve performance by loading large messages into a mutable structure then converting to an immutable one.

So I want a "generic" mutable container for deserializing into.

Extensible does this with Map (FieldId) ExtFieldValue
Unknown does this with Seq UnknownFieldValue

So I can separate loading and parsing messages?
Use IntMap for simplicity for now.

When loading a message: get wiretag (int32) and put bytestring using same catch'Unknown -> loadUnknown -> wireGetFromWire method.

Put all these into IntMap from fieldId to Seq Bytestring like unknown.
This has logarithmic insert time.  It ought to be wicked fast.

Then pass over this with knowledge of message, but in a viciously simple way.  Each field of the message is built by looking up the field id and interpreting it.


makeFrom map = MyMessage (reqPrimitive lookup fid1 map) -- picks out last
                         (optPrimitive lookup fid2 map) -- picks out last
                         (repPrimitive lookup fid3 map) -- does each one
                         (reqMessage lookup fid4 map) -- concatenate BS then create
                         (optMessage lookup fid5 map) -- concatenate BS then create
                         (repMessage lookup fid6 map) -- does each one
                         (reqKey key7 lookup fid7 map)
                         (optKey key8 lookup fid8 map)
                         (repKey key9 lookup fid9 map)
                         (collectOtherExt map)
                         (collectUnknown map)

see applyGet in Extensions.hs for help h

APPLICATIVE FOR THE WIN

-}

module Text.ProtocolBuffers.Generic
    ( GenMessage(genMessage)
    , KeyRing(KeyRing)
    , KeyGen
    , Gen
    , getMessage
    , reqPrimitive
    , optPrimitive
    , rep1Primitive -- primitives that cannot be packed such as string and bytes
    , rep2Primitive -- primitives that can be packed
    , reqMessage
    , optMessage
    , repMessage
    , reqGroup
    , optGroup
    , repGroup
    , genUnknownField
    , genExtField
    , genUnknownField'1
    , genUnknownField'2
    , optKey
    , repKey
    , unpackedKey
    , packedKey
    )
    where

import Data.IntMap(IntMap)
import qualified Data.IntMap as I
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(foldl')

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Get
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Unknown
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.WireMessage
import Data.Sequence((><),ViewR(EmptyR,(:>)))
import qualified Data.Sequence as Seq
import Control.Monad(forM)
import Control.Monad.Error.Class(MonadError(throwError),Error(strMsg))
import Data.Traversable(traverse)
import Data.Foldable(fold,msum,toList,foldlM)
import qualified Data.ByteString.Lazy as L
import Control.Applicative((<$>),(<*>))
import Data.Typeable(cast,Typeable)

--import Debug.Trace(trace)
trace :: a -> b -> b
trace _ s = s

-- Gen is a map from WireTag
type Gen = IntMap (Seq ByteString)

-- Wrapped parseWireExtMaybe parseWireExtSeq parseWireExtPackedSeq
type KeyGen = (Gen -> Either String (Maybe (FieldId,ExtFieldValue)))

-- Use phantom message parameter to capture message type
data KeyRing = KeyRing [KeyGen] [(FieldId,FieldId)] (Set WireTag)
  deriving (Typeable)


class (Wire message, Mergeable message, ReflectDescriptor message) => GenMessage message where
  genMessage :: Gen -> Either String message

getMessage :: (GenMessage message) => Get message
getMessage = {-# getMessage #-} do
  messageLength <- getVarInt
  start <- bytesRead
  let stop = messageLength+start
      go g = do
        here <- bytesRead
        case compare stop here of
          EQ -> trace (":getMessage: "++show g) $
                case genMessage g of
                  Left err -> throwError ("getMessage: "++err)
                  Right message -> return message
          LT -> error "overran data"
          GT -> do
            wireTag <- getVarInt
            let (fieldId,wireType) = splitWireTag (WireTag wireTag)
            bs <- wireGetFromWire fieldId wireType
            trace (":getMessage: "++show ((messageLength,start,stop,here),(wireTag,fieldId,wireType),L.length bs)) $ 
              go (I.insertWith (\new old -> old >< new) (fromEnum wireTag) (Seq.singleton bs) g)
  go I.empty

-- Skip wireType of 4 so that concatenated groups are processed just like concatenated messages
getMessages :: (GenMessage message) => Get message
getMessages = {-# getMessages #-} go I.empty where
  go gen = do
    messageLength <- getVarInt
    start <- bytesRead
    let stop = start + messageLength
        loop g = do
          here <- bytesRead
          case compare stop here of
            EQ -> do
              done <- isEmpty
              case done of
                False -> go g
                True -> trace ("getMessages: "++show g) $ do
                  case genMessage g of
                    Left err -> throwError ("getMessages: "++err)
                    Right message -> return message
            LT -> error "overran data"
            GT -> do
              wireTag <- getVarInt
              let (fieldId,wireType) = splitWireTag (WireTag wireTag)
              bs <- wireGetFromWire fieldId wireType
              trace (":getMessages: "++show ((messageLength,start,stop,here),(wireTag,fieldId,wireType),L.length bs,bs)) $ 
                loop (I.insertWith (\new old -> old >< new) (fromEnum wireTag) (Seq.singleton bs) g)
    loop gen

getBareGroup :: (GenMessage message) => Get message
getBareGroup = go I.empty where
  go g = do
    done <- isEmpty
    case done of
      False -> do
        wireTag <- getVarInt
        let (fieldId,wireType) = splitWireTag (WireTag wireTag)
        if 4/=wireType
          then do
            bs <- wireGetFromWire fieldId wireType
            trace (":getBareMessage: "++show ((wireTag,fieldId,wireType),L.length bs)) $
              go (I.insertWith (\new old -> old >< new) (fromEnum wireTag) (Seq.singleton bs) g)
          else
            go g
      True -> trace (":getBareMessage: "++show g) $
        case genMessage g of
          Left err -> throwError ("getBareMessage: "++err)
          Right message -> return message

rightmost :: Seq ByteString -> Either String ByteString
rightmost s = case Seq.viewr s of
                EmptyR -> throwError (strMsg "impossible")
                _ :> val -> return val

-- parse* shared by req/opt/rep variants

parseOne :: (Show t,Wire t) => WireTag -> FieldType -> ByteString -> Either String t
parseOne wt ft bs = trace (":parseOne: "++show (wt,ft,L.length bs, bs)) $ {-# SCC "parseOne" #-}
  case runGetAll (wireGet ft) bs of
    Failed _i _s -> throwError "parse failed"
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parseOne: parse underran "++show ((wt,ft),(i,L.length bs,L.length bs'),r)
    Partial {} -> throwError "parse overran" -- impossible case due to runGetAll

parsePacked :: (Show t,Wire t) => WireTag -> FieldType -> ByteString -> Either String (Seq t)
parsePacked wt ft bs =  {-# SCC "parsePacked" #-}
  case runGetAll (genericPacked ft) bs of -- This might work on Enum....or not?
    Failed _i _s -> throwError "parse failed"
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parsePacked: parse underran "++show ((wt,ft),(i,L.length bs,L.length bs'),r)
    Partial {} -> throwError "parse overran" -- impossible case due to runGetAll

parseOneMessage :: (GenMessage message) => ByteString -> Either String message
parseOneMessage bs = trace (":parseOneMessage: "++show (L.length bs,bs)) $ {-# SCC "parseOneMessage" #-}
  case runGetAll (wireGet 11) {-XXXgetMessages-} bs of
    Failed i s -> throwError $ "parseOneMessage: parse failed "++show (i,s)
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parseOneMessage: parse underran "++show (i,L.length bs,L.length bs')
    Partial {} -> throwError $ "parseOneMessage: parse overran" -- impossible case due to runGetAll

parseOneGroup :: (GenMessage message) => ByteString -> Either String message
parseOneGroup bs = trace (":parseOneGroup: "++show (L.length bs,bs)) $ {-# SCC "parseOneGroup" #-}
  case runGetAll getBareGroup bs of
    Failed i s -> throwError $ "parseOneGroup: parse failed "++show (i,s)
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parseOneGroup: parse underran "++show (i,L.length bs,L.length bs')
    Partial {} -> throwError $ "parseOneGroup: parse overran" -- impossible case due to runGetAll

-- all *Primitive do work on Enum types

reqPrimitive :: (Show t,Wire t) => WireTag -> FieldType -> Gen -> Either String t
reqPrimitive wt ft gen = 
  case I.lookup (fromEnum wt) gen of
    Nothing -> throwError "missing required field"
    Just s -> parseOne wt ft =<< rightmost s

optPrimitive :: (Show t,Wire t) => WireTag -> FieldType -> Gen -> Either String (Maybe t)
optPrimitive wt ft gen = {-# SCC "optPrimitive" #-}
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOne wt ft =<< rightmost s)

rep1Primitive :: (Show t,Wire t) => WireTag -> FieldType -> Gen -> Either String (Seq t)
rep1Primitive wt ft gen = {-# SCC "rep1Primitive" #-}
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse (parseOne wt ft) s

rep2Primitive :: (Show t,Wire t) => WireTag -> WireTag -> FieldType -> Gen -> Either String (Seq t)
rep2Primitive wt1 wt2 ft gen = {-# SCC "rep2Primitive" #-} ((><) <$> repUnpacked <*> repPacked)
 where
  repUnpacked :: (Show t,Wire t) => Either String (Seq t)
  repUnpacked =
    case I.lookup (fromEnum wt1) gen of
      Nothing -> return Seq.empty
      Just s -> traverse (parseOne wt1 ft) s

  repPacked :: forall t. (Show t,Wire t) => Either String (Seq t)
  repPacked =
    case I.lookup (fromEnum wt2) gen of
      Nothing -> return Seq.empty
      Just (s :: Seq ByteString) -> let x :: Either String (Seq  (Seq t))
                                        x = traverse (parsePacked wt1 ft) s
                                    in fmap msum x
                                        
reqMessage :: (GenMessage message) => WireTag -> Gen -> Either String message
reqMessage wt gen = {-# SCC "reqMessage" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> throwError "missing required message field"
    Just s -> parseOneMessage (fold s)

optMessage :: (GenMessage message) => WireTag -> Gen -> Either String (Maybe message)
optMessage wt gen = trace (":optMessage: "++show wt) $ {-# SCC "optMessage" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOneMessage (fold s))

repMessage :: (GenMessage message) => WireTag -> Gen -> Either String (Seq message)
repMessage wt gen = {-# SCC "repMessage" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse parseOneMessage s

reqGroup :: (GenMessage message) => WireTag -> Gen -> Either String message
reqGroup wt gen = {-# SCC "reqGroup" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> throwError "missing required message field"
    Just s -> parseOneGroup (fold s)

optGroup :: (GenMessage message) => WireTag -> Gen -> Either String (Maybe message)
optGroup wt gen = trace (":optGroup: "++show wt) $ {-# SCC "optGroup" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOneGroup (fold s))

repGroup :: (GenMessage message) => WireTag -> Gen -> Either String (Seq message)
repGroup wt gen = {-# SCC "repGroup" #-} 
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse parseOneGroup s

-- add extensions and/or unknown later
genUnknownField :: (GenMessage message) => message -> Gen -> Either String UnknownField
genUnknownField _ _gen = Right defaultValue

{-
genExtField :: (GenMessage message) => message -> Gen -> Either String ExtField
genExtField this =
  let -- (GetMessageInfo { allowedTags }) = getMessageInfo this
      -- inRecognized x = Set.member x allowedTags
      (DescriptorInfo { knownKeys, extRanges }) = reflectDescriptorInfo this
      keys :: Map FieldId FieldInfo
      keys = Map.fromList . map (\fi -> (fieldNumber fi,fi)) . toList $ knownKeys
      inExtRanges x = any (\(lo,hi) -> lo<=x && x <=hi) extRanges
      wt2fi :: (Int,Seq ByteString) -> (FieldId,(WireTag,Seq ByteString,Maybe FieldInfo))
      wt2fi (wt,s) = let wt' = WireTag . toEnum $ wt
                         fid = fst . splitWireTag $ wt'
                     in ( fid, (wt',s,Map.lookup fid keys) )
      f :: Map FieldId ExtFieldValue
        -> (FieldId,(WireTag,Seq ByteString,Maybe FieldInfo))
        -> Either String (Map FieldId ExtFieldValue)
      f ef (fid,(wt,s,Nothing)) = 
          let (_,wireTag) = splitWireTag wt
              eps = fmap (EP wireTag) s
          in case Map.lookup fid ef of
               Nothing -> return $! Map.insert fid (ExtFromWire eps) ef
               Just (ExtFromWire raw) -> let v' = ExtFromWire (raw >< eps)
                                         in seq v' $ return $! (Map.insert fid v' ef)
               _ -> throwError "genExtField.f: unexpected case, should not have known and unknown fid!"
      f m (fid,(wt,s,Just (FieldInfo {..}))) = return m
  in \ gen -> fmap ExtField
              . foldlM f Map.empty
              . filter (inExtRanges . fst)
              . map wt2fi
              $ I.toList gen
-}

genUnknownField'1 :: Set WireTag -> (Gen -> Either String UnknownField)
genUnknownField'1 known = {-# SCC "genUnknownField'1" #-} 
  let wrap (wt,s) = (WireTag (toEnum wt),s)
      raw (wt,_) = Set.notMember wt known 
      g2u (wt,s) = fmap (UFV wt) s
  in \gen -> let s1 = msum . map g2u . filter raw . map wrap . I.toList $ gen
             in return (UnknownField s1)

genUnknownField'2 :: KeyRing -> (Gen -> Either String UnknownField)
genUnknownField'2 (KeyRing keyGens extRanges known) = {-# SCC "genUnknownField'2" #-} 
  let wrap (wt,s) = (fid,wt',s) where wt' = WireTag (toEnum wt)
                                      (fid,_) = splitWireTag wt'
      raw (fid,wt,_) = Set.notMember wt known && not (any (\(lo,hi) -> lo<=fid && fid <=hi) extRanges)
      g2u (_,wt,s) = fmap (UFV wt) s
  in \gen -> let s1 = msum . map g2u . filter raw . map wrap . I.toList $ gen
             in return (UnknownField s1)

genExtField :: KeyRing -> (Gen -> Either String ExtField)
genExtField (KeyRing keyGens extRanges known) = {-# SCC "genExtField" #-} 
  let wrap (wt,s) = (fid,wireType,wt',s) where wt' = WireTag (toEnum wt)
                                               (fid,wireType) = splitWireTag wt'
      raw (fid,_,wt,_) = Set.notMember wt known && any (\(lo,hi) -> lo<=fid && fid <=hi) extRanges
      g2p (fid,wireTag,_,s) = (fid, ExtFromWire (fmap (EP wireTag) s))
  in \gen -> do
        maybePairs <- forM keyGens ($ gen)
        let m0 = Map.fromList [ pair | Just pair <- maybePairs ]
            m1 = Map.fromList (map g2p . filter raw . map wrap . I.toList $ gen)
        return (ExtField (Map.union m0 m1))

optKey :: Key Maybe msg v -> KeyGen
optKey key@(Key fid ft _) =
  let wireType = toWireType ft
      wireTag = mkWireTag fid wireType
      parse = parseWireExtMaybe key wireType
  in \gen ->
    case I.lookup (fromEnum wireTag) gen of
      Nothing -> return Nothing
      Just s ->
        case parse (fmap (EP wireType) s) of
          Left err -> throwError err
          Right val -> return (Just val)

-- Repeated Key for field type that cannot be packed
repKey :: Key Seq msg v -> KeyGen
repKey key@(Key fid ft _) =
  let wireType = toWireType ft
      wireTag = mkWireTag fid wireType
      parse = parseWireExtSeq key wireType
  in \gen ->
    case I.lookup (fromEnum wireTag) gen of
      Nothing -> return Nothing
      Just s ->
        case parse (fmap (EP wireType) s) of
          Left err -> throwError err
          Right val -> return (Just val)

-- unpackedKey is for unpacked repeated field that might have to handle packed data on wire.
-- Ultimately store in ExtRepeated.
unpackedKey :: forall msg v . Key Seq msg v -> KeyGen
unpackedKey normalKey@(Key fid ft dv) =
  let wireType = toWireType ft
      normalTag = mkWireTag fid wireType
      packedTag = toPackedWireTag fid
      packedKey :: Key PackedSeq msg v -- fake a packed version of the key
      packedKey = Key fid ft dv
      parseNormal = parseWireExtSeq normalKey wireType
      parsePacked = parseWireExtPackedSeq packedKey wireType
  in \gen -> do
       normal <- case I.lookup (fromEnum normalTag) gen of
                   Nothing -> return Nothing
                   Just s ->
                     case parseNormal (fmap (EP wireType) s) of
                       Left err -> throwError err
                       Right val -> return (Just val)
       packed <- case I.lookup (fromEnum packedTag) gen of
                   Nothing -> return Nothing
                   Just s ->
                     case parsePacked (fmap (EP wireType) s) of
                       Left err -> throwError err
                       Right val -> return (Just val)
       case (normal,packed) of
         (Nothing,Nothing) -> return Nothing
         (Just {},Nothing) -> return normal
         (Nothing,Just (i,ExtPacked ft ds2)) ->return (Just (i,ExtRepeated ft ds2))
         ( Just (i,ExtRepeated ft (GPDynSeq x@GPWitness s1))
           , Just (_,ExtPacked   _  (GPDynSeq GPWitness s2)) ) ->
             case (cast s1, cast s2) of
               (Just t1, Just t2) -> return (Just (i,ExtRepeated ft (GPDynSeq x (t1 >< t2))))
               _ -> throwError $ "Text.ProtocolBuffers.Generic.unpackedKey: cast failed! " ++ show (normalKey,i,ft)
         _ -> throwError $ "Text.ProtocolBuffers.Generic.unpackedKey: unexpected case! " ++ show normalKey

-- unpackedKey is for packed repeated field that always have to handle unpacked data on wire.
-- Ultimately store in ExtPacked.
packedKey :: forall msg v. Key PackedSeq msg v -> KeyGen
packedKey packedKey@(Key fid ft dv) = 
  let wireType = toWireType ft
      normalTag = mkWireTag fid wireType
      packedTag = toPackedWireTag fid
      normalKey :: Key Seq msg v -- fake a normal version of the key
      normalKey = Key fid ft dv
      parseNormal = parseWireExtSeq normalKey wireType
      parsePacked = parseWireExtPackedSeq packedKey wireType
  in \gen -> do
       normal <- case I.lookup (fromEnum normalTag) gen of
                   Nothing -> return Nothing
                   Just s ->
                     case parseNormal (fmap (EP wireType) s) of
                       Left err -> throwError err
                       Right val -> return (Just val)
       packed <- case I.lookup (fromEnum packedTag) gen of
                   Nothing -> return Nothing
                   Just s ->
                     case parsePacked (fmap (EP wireType) s) of
                       Left err -> throwError err
                       Right val -> return (Just val)
       case (normal,packed) of
         (Nothing,Nothing) -> return Nothing
         (Just (i,ExtRepeated ft ds1),Nothing) -> return (Just (i,ExtPacked ft ds1))
         (Nothing,Just {}) ->return packed
         ( Just (i,ExtRepeated ft (GPDynSeq x@GPWitness s1))
           , Just (_,ExtPacked   _  (GPDynSeq GPWitness s2)) ) ->
             case (cast s1, cast s2) of
               (Just t1, Just t2) -> return (Just (i,ExtPacked ft (GPDynSeq x (t1 >< t2))))
               _ -> throwError $ "Text.ProtocolBuffers.Generic.packedKey: cast failed! " ++ show (packedKey,i,ft)
         _ -> throwError $ "Text.ProtocolBuffers.Generic.packedKey: unexpected case! " ++ show packedKey

-- current getting for keys has 4 flavors:
-- wireGetKey for known keys and the expected WireTag
-- wireGetKeyToPacked for known packed repeated keys and parsing unpacked WireTag
-- wireGetKeyToUnPacked for known not-packed keys and parsing packed WireTag
-- loadExtension for parsing WireTags for no known key
--   this last one takes all tags not in allowedTags but still in extRanges
--
-- Unknown takes all tags not in allowedTags and not in extRanges
