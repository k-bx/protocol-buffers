{-# LANGUAGE RankNTypes,ScopedTypeVariables #-}
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
    )
    where

import Data.IntMap(IntMap)
import qualified Data.IntMap as I

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Get
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Unknown
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.WireMessage
import Data.Sequence((><),ViewR(EmptyR,(:>)))
import qualified Data.Sequence as Seq
import Control.Monad.Error.Class(MonadError(throwError),Error(strMsg))
import Data.Traversable(traverse)
import Data.Foldable(fold,msum)
import qualified Data.ByteString.Lazy as L
import Control.Applicative((<$>),(<*>))

import Debug.Trace(trace)

type Gen = IntMap (Seq ByteString)

class (Mergeable message, ReflectDescriptor message) => GenMessage message where
  genMessage :: Gen -> Either String message

getMessage :: (GenMessage message) => Get message
getMessage = do
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
getMessages = go I.empty where
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
parseOne wt ft bs = trace (":parseOne: "++show (wt,ft,L.length bs, bs)) $ do
  case runGetAll (wireGet ft) bs of
    Failed _i _s -> throwError "parse failed"
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parseOne: parse underran "++show ((wt,ft),(i,L.length bs,L.length bs'),r)
    Partial {} -> throwError "parse overran" -- impossible case due to runGetAll

parsePacked :: (Show t,Wire t) => WireTag -> FieldType -> ByteString -> Either String (Seq t)
parsePacked wt ft bs = do
  case runGetAll (genericPacked ft) bs of -- This might work on Enum....or not?
    Failed _i _s -> throwError "parse failed"
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parsePacked: parse underran "++show ((wt,ft),(i,L.length bs,L.length bs'),r)
    Partial {} -> throwError "parse overran" -- impossible case due to runGetAll

parseOneMessage :: (GenMessage message) => ByteString -> Either String message
parseOneMessage bs = trace (":parseOneMessage: "++show (L.length bs,bs)) $
  case runGetAll getMessages bs of
    Failed i s -> throwError $ "parseOneMessage: parse failed "++show (i,s)
    Finished bs' i r | L.null bs' -> return r
                     | otherwise -> throwError $ "parseOneMessage: parse underran "++show (i,L.length bs,L.length bs')
    Partial {} -> throwError $ "parseOneMessage: parse overran" -- impossible case due to runGetAll

parseOneGroup :: (GenMessage message) => ByteString -> Either String message
parseOneGroup bs = trace (":parseOneGroup: "++show (L.length bs,bs)) $
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
optPrimitive wt ft gen = 
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOne wt ft =<< rightmost s)

rep1Primitive :: (Show t,Wire t) => WireTag -> FieldType -> Gen -> Either String (Seq t)
rep1Primitive wt ft gen =
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse (parseOne wt ft) s

rep2Primitive :: (Show t,Wire t) => WireTag -> WireTag -> FieldType -> Gen -> Either String (Seq t)
rep2Primitive wt1 wt2 ft gen = (><) <$> repUnpacked <*> repPacked
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
                                        
-- | Handles both messages and groups
reqMessage :: (GenMessage message) => WireTag -> Gen -> Either String message
reqMessage wt gen =
  case I.lookup (fromEnum wt) gen of
    Nothing -> throwError "missing required message field"
    Just s -> parseOneMessage (fold s)

-- | Handles both messages and groups
optMessage :: (GenMessage message) => WireTag -> Gen -> Either String (Maybe message)
optMessage wt gen = trace (":optMessage: "++show wt) $
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOneMessage (fold s))

-- | Handles both messages and groups
repMessage :: (GenMessage message) => WireTag -> Gen -> Either String (Seq message)
repMessage wt gen =
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse parseOneMessage s

-- | Handles both messages and groups
reqGroup :: (GenMessage message) => WireTag -> Gen -> Either String message
reqGroup wt gen =
  case I.lookup (fromEnum wt) gen of
    Nothing -> throwError "missing required message field"
    Just s -> parseOneGroup (fold s)

-- | Handles both messages and groups
optGroup :: (GenMessage message) => WireTag -> Gen -> Either String (Maybe message)
optGroup wt gen = trace (":optGroup: "++show wt) $
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Nothing
    Just s -> fmap Just (parseOneGroup (fold s))

-- | Handles both messages and groups
repGroup :: (GenMessage message) => WireTag -> Gen -> Either String (Seq message)
repGroup wt gen =
  case I.lookup (fromEnum wt) gen of
    Nothing -> return Seq.empty
    Just s -> traverse parseOneGroup s

-- add extensions and/or unknown later
genUnknownField :: (GenMessage message) => message -> Gen -> Either String UnknownField
genUnknownField _ _gen = Right defaultValue

genExtField :: (GenMessage message) => message -> Gen -> Either String ExtField
genExtField _ _gen = Right defaultValue
