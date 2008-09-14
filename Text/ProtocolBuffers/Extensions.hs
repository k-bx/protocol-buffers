module Text.ProtocolBuffers.Extensions(ExtKey(..),MessageAPI(..),defaultKeyValue
                                      ,wireSizeExtField,wirePutExtField,getMessageExt,getBareMessageExt
                                      ,GPB,Key(..),ExtField,ExtendMessage(..)
                                      ) where

import Data.Map(Map)
import Data.Dynamic
import Data.Generics
import Data.List(intersperse,foldl')
import Data.Maybe(catMaybes,fromMaybe,fromJust)
import Data.Typeable
import Data.Monoid(mempty,mappend)
import Data.Sequence(Seq,(|>),viewr,ViewR(..))
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import qualified Data.Map as M

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Default
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Get as Get (Get,runGet,Result(..),lookAhead,getLazyByteString,spanOf,skip,bytesRead)

err msg = error $ "Text.ProtocolBuffers.Extensions error\n"++msg

-- | A key type (opaque) that has a phantom type of Maybe or Seq that
-- corresponds to Optional or Repated. And a second phantom type that
-- matches the message type it must be used with.  It also uses a GADT
-- to put all the needed class instances into scope.  The actual
-- content is the FieldId (that numeric key), the FieldType (for
-- sanity checks), and Maybe v (a non-standard default value).
data Key c msg v where
  Key :: (ExtKey c,ExtendMessage msg,GPB v) => FieldId -> FieldType -> (Maybe v) -> Key c msg v

instance Typeable1 c => Typeable2 (Key c) where
  typeOf2 _ = mkTyConApp (mkTyCon "Text.ProtocolBuffers.Extensions.Key") [typeOf1 (undefined :: c ())]

instance (Typeable1 c, Typeable msg, Typeable v) => Show (Key c msg v) where
  show key@(Key fieldId fieldType maybeDefaultValue) =
    concat ["(Key (",show fieldId
           ,") (",show fieldType
           ,") (",show maybeDefaultValue
           ,") :: ",show (typeOf key)
           ,")"]

-- | The GPWitness is an instance witness for the GPB classes.  This
-- exists mainly to be a part of GPDyn.
data GPWitness a where GPWitness :: (GPB a) => GPWitness a
  deriving (Typeable)

-- | The GPDyn is my specialization of Dynamic.  It hides the type
-- with an existential, but the GPWitness brings the class instances
-- into scope.  This witness can be gleaned from a Key.
data GPDyn = forall a . GPDyn (GPWitness a) a
  deriving (Typeable)

-- | The GPDynSeq is another specialization of Dynamic.
data GPDynSeq = forall a . GPDynSeq (GPWitness a) (Seq a)
  deriving (Typeable)

-- | The WireType is used to ensure the Seq is homogenous.
-- The ByteString is the unparsed input after the tag.
-- The WireSize includes all tags.
data ExtFieldValue = ExtFromWire WireType (Seq ByteString)
                   | ExtOptional FieldType GPDyn
                   | ExtRepeated FieldType GPDynSeq
  deriving (Typeable,Eq,Ord,Show)

-- | ExtField is a newtype'd map from the numeric FieldId key to the
-- ExtFieldValue.  This allows for the needed class instances.
newtype ExtField = ExtField (Map FieldId ExtFieldValue)
  deriving (Typeable,Eq,Ord,Show)

class Typeable msg => ExtendMessage msg where
  getExtField :: msg -> ExtField
  putExtField :: ExtField -> msg -> msg
  validExtRanges :: msg -> [(FieldId,FieldId)]
-- countExtField :: msg -> Int
-- hasFromWire :: msg -> Bool

class ExtKey c where
  -- | Change or clear the value of a key in a message. Passing
  -- 'Nothing' to a optional key or an empty sequence to a repeated
  -- key clears the value.  This function thus maintains the invariant
  -- that have a field number in the extension map means that the
  -- field is set and not empty.
  putExt :: Key c msg v -> c v -> msg -> msg
  -- | Access the key in the message.  Optional have type @(Key Maybe
  -- msg v)@ and return type @(Maybe v)@ while repeated fields have type
  -- @(Key Seq msg v)@ and return type @(Seq v)@.
  --
  -- There are a few sources of errors with the lookup of the key:
  --  * It may find unparsed bytes from loading the message. 'getExt' will attempt to parse the bytes as the key\'s value type, and may fail.  The parsing is done with the 'parseWireExt' method (not exported to user API).
  --  * The wrong optional versus repeated type is a failure
  --  * The wrong type of the value might be found in the map and cause a failure
  --
  -- All of the above should only happen if an incompatible key was used.
  getExt :: Key c msg v -> msg -> Either String (c v)
  clearExt :: Key c msg v -> msg -> msg
  parseWireExt :: Key c msg v -> WireType -> Seq ByteString -> Either String (FieldId,ExtFieldValue)
  wireGetKey :: Key c msg v -> msg -> Get msg

-- | This can return the default value for a given Key
defaultKeyValue :: Key c msg v -> v
defaultKeyValue (Key _ _ md) = fromMaybe defaultValue md

-- | The Key and GPWitness GADTs use GPB as a shorthand for many classes
class (Mergeable a,Default a,Wire a,Show a,Typeable a,Eq a,Ord a) => GPB a 

instance GPB Bool
instance GPB ByteString
instance GPB Utf8
instance GPB Double
instance GPB Float
instance GPB Int32
instance GPB Int64
instance GPB Word32
instance GPB Word64

instance Mergeable ExtField where
  mergeEmpty = ExtField M.empty
  mergeAppend (ExtField m1) (ExtField m2) = ExtField (M.unionWith mergeExtFieldValue m1 m2)

mergeExtFieldValue (ExtFromWire wt1 s1) (ExtFromWire wt2 s2) =
  if wt1 /= wt2 then err $ "mergeExtFieldValue : ExtFromWire WireType mismatch " ++ show (wt1,wt2)
    else ExtFromWire wt2 (mappend s1 s2)

mergeExtFieldValue (ExtOptional ft1 (GPDyn GPWitness d1))
                   (ExtOptional ft2 (GPDyn GPWitness d2)) =
  if ft1 /= ft2 then err $ "mergeExtFieldValue : ExtOptional FieldType mismatch "++show (ft1,ft2)
    else case cast d2 of
           Nothing -> err $ "mergeExtFieldValue : ExtOptional cast failed, FieldType "++show (ft2,typeOf d1,typeOf d2)
           Just d2' -> ExtOptional ft2 (GPDyn GPWitness (mergeAppend d1 d2'))

mergeExtFieldValue (ExtRepeated ft1 (GPDynSeq GPWitness s1))
                   (ExtRepeated ft2 (GPDynSeq GPWitness s2)) =
  if ft1 /= ft2 then err $ "mergeExtFieldValue : ExtRepeated FieldType mismatch "++show (ft1,ft2)
    else case cast s2 of
           Nothing -> err $ "mergeExtFieldValue : ExtRepeated cast failed, FieldType "++show (ft2,typeOf s1,typeOf s2)
           Just s2' -> ExtRepeated ft2 (GPDynSeq GPWitness (mappend s1 s2'))

instance Default ExtField where
  defaultValue = ExtField M.empty

instance Show (GPWitness a) where
  showsPrec n GPWitness = ("(GPWitness :: GPWitness ("++) . shows (typeOf (undefined :: a)) . (')':) . (')':)

instance Eq (GPWitness a) where
  (==) GPWitness GPWitness = True
  (/=) GPWitness GPWitness = False

instance Ord (GPWitness a) where
  compare GPWitness GPWitness = EQ

instance (GPB a) => Data (GPWitness a) where
  gunfold k z c = case constrIndex c of
                    1 -> z GPWitness
                    _ -> error "gunfold of GPWitness error"
  toConstr GPWitness = gpWitnessC
  dataTypeOf _ = gpWitnessDT

gpWitnessC = mkConstr gpWitnessDT "GPWitness" [] Prefix 
gpWitnessDT = mkDataType "GPWitness" [gpWitnessC]

gpDynC = mkConstr gpDynDT "GPDyn" ["a"] Prefix
gpDynDT = mkDataType "GPDyn" [gpDynC]

fromGPDyn :: (GPB a) => GPDyn -> Maybe a
fromGPDyn (GPDyn GPWitness a) = cast a

typeOfGPDyn :: GPDyn -> TypeRep
typeOfGPDyn (GPDyn GPWitness a) = typeOf a

defaultValueGPDyn :: GPWitness a -> GPDyn
defaultValueGPDyn x@GPWitness = GPDyn x defaultValue

mergeEmptyGPDyn :: GPWitness a -> GPDyn
mergeEmptyGPDyn x@GPWitness = GPDyn x mergeEmpty

mergeAppendGPDyn :: GPDyn -> GPDyn -> Maybe GPDyn
mergeAppendGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (GPDyn GPWitness . mergeAppend a1) (cast a2)

instance Eq GPDyn where
  (==) a b = fromMaybe False (eqGPDyn a b)

instance Ord GPDyn where
  compare a b = fromMaybe (compare (show a) (show b)) (ordGPDyn a b)

instance Show GPDyn where
  showsPrec n (GPDyn x@GPWitness a) = ("(GPDyn "++) . shows x . (" ("++) . shows a . ("))"++)

instance Eq GPDynSeq where
  (==) a b = fromMaybe False (eqGPDynSeq a b)

instance Ord GPDynSeq where
  compare a b = fromMaybe (compare (show a) (show b)) (ordGPDynSeq a b)

instance Show GPDynSeq where
  showsPrec n (GPDynSeq x@GPWitness s) = ("(GPDynSeq "++) . shows x . (" ("++) . shows s . ("))"++)

ordGPDyn :: GPDyn -> GPDyn -> Maybe Ordering
ordGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (compare a1) (cast a2)

eqGPDyn :: GPDyn -> GPDyn -> Maybe Bool
eqGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (a1==) (cast a2)

showGPDyn :: GPDyn -> String
showGPDyn (GPDyn GPWitness s) = show s

ordGPDynSeq :: GPDynSeq -> GPDynSeq -> Maybe Ordering
ordGPDynSeq (GPDynSeq GPWitness a1) (GPDynSeq GPWitness a2) = fmap (compare a1) (cast a2)

eqGPDynSeq :: GPDynSeq -> GPDynSeq -> Maybe Bool
eqGPDynSeq (GPDynSeq GPWitness a1) (GPDynSeq GPWitness a2) = fmap (a1==) (cast a2)

showGPDynSeq :: GPDynSeq -> String
showGPDynSeq (GPDynSeq GPWitness s) = show s

--

wireSizeGPDyn :: FieldType -> GPDyn -> WireSize
wireSizeGPDyn ft (GPDyn GPWitness a) = wireSize ft a 

wirePutGPDyn :: FieldType -> GPDyn -> Put
wirePutGPDyn ft (GPDyn GPWitness a) = wirePut ft a 

wireGetGPDyn :: forall a. GPWitness a -> FieldType -> Get GPDyn
wireGetGPDyn GPWitness ft = fmap (GPDyn GPWitness) (wireGet ft :: Get a)

getWitness :: (GPB a) => GPDyn -> Maybe (GPWitness a)
getWitness (GPDyn x@GPWitness _) = cast x

readGPDyn :: forall a . Read a => GPWitness a -> String -> GPDyn
readGPDyn x@(GPWitness) s =
  let t :: a; t = read s
  in GPDyn x t

instance ExtKey Maybe where
  putExt key Nothing msg = clearExt key msg
  putExt (Key i t _) (Just v) msg =
    let (ExtField ef) = getExtField msg
        v' = ExtOptional t (GPDyn GPWitness v)
        ef' = M.insert i v' ef
    in seq v' $ seq ef' (putExtField (ExtField ef') msg)

  clearExt (Key i _ _ ) msg =
    let (ExtField ef) = getExtField msg
        ef' = M.delete i ef
    in seq ef' (putExtField (ExtField ef') msg)

  getExt k@(Key i t _) msg =
    let (ExtField ef) = getExtField msg
    in case M.lookup i ef of
         Nothing -> Right Nothing
         Just (ExtFromWire wt raw) -> either Left (getExt' . snd) (parseWireExt k wt raw)
         Just x -> getExt' x
   where getExt' (ExtRepeated t' _) = Left $ "getKey Maybe: ExtField has repeated type: "++show (k,t')
         getExt' (ExtOptional t' (GPDyn GPWitness d)) | t/=t' =
           Left $ "getExt Maybe: Key's FieldType does not match ExtField's: "++show (k,t')
                                                      | otherwise =
           case cast d of
             Nothing -> Left $ "getExt Maybe: Key's value cast failed: "++show (k,typeOf d)
             Just d' -> Right (Just d')

  parseWireExt k@(Key fi ft mv)  wt raw | wt /= toWireType ft =
    Left $ "parseWireExt Maybe: Key's FieldType does not match ExtField's wire type: "++show (k,toWireType ft,wt)
                                        | otherwise = do
    let mkWitType :: Maybe a -> GPWitness a
        mkWitType = undefined
        witness = GPWitness `asTypeOf` (mkWitType mv)
        parsed = map (applyGet (wireGet ft)) . F.toList $ raw
        errs = [ m | Left m <- parsed ]
    if null errs then Right (fi,(ExtOptional ft (GPDyn witness (mergeConcat [ a | Right a <- parsed ]))))
      else Left (unlines errs)

  wireGetKey (Key i t mv) msg = do
    let myCast :: Maybe a -> Get a
        myCast = undefined
    v <- wireGet t `asTypeOf` (myCast mv)
    let (ExtField ef) = getExtField msg
        v' = ExtOptional t (GPDyn GPWitness v)
        ef' = M.insert i v' ef
    seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)

applyGet :: Get.Get r -> ByteString -> Either String r
applyGet g bs = resolveEOF (runGet g bs) where
  resolveEOF :: Get.Result r -> Either String r
  resolveEOF (Get.Failed i s) = Left ("Failed at "++show i++" : "++s)
  resolveEOF (Get.Finished bs i r) | L.null bs = Right r
                                   | otherwise = Left "Not all input consumed"
  resolveEOF (Get.Partial {}) = Left "Not enough input"

instance ExtKey Seq where
  putExt key@(Key i t _) s msg | Seq.null s = clearExt key msg
                               | otherwise =
      let (ExtField ef) = getExtField msg
          v' = ExtRepeated t (GPDynSeq GPWitness s)
          ef' = M.insert i v' ef
      in seq v' $ seq ef' (putExtField (ExtField ef') msg)

  clearExt (Key i _ _ ) msg =
    let (ExtField ef) = getExtField msg
        ef' = M.delete i ef
    in seq ef' (putExtField (ExtField ef') msg)

  getExt k@(Key i t _) msg =
    let (ExtField ef) = getExtField msg
    in case M.lookup i ef of
         Nothing -> Right Seq.empty
         Just (ExtFromWire wt raw) -> either Left (getExt' . snd) (parseWireExt k wt raw) 
   where getExt' (ExtOptional t' _) = Left $ "getKey Seq: ExtField has optional type: "++show (k,t')
         getExt' (ExtRepeated t' (GPDynSeq GPWitness s)) | t'/=t =
           Left $ "getExt Seq: Key's FieldType does not match ExtField's: "++show (k,t')
                                                         | otherwise =
           case cast s of
             Nothing -> Left $ "getExt Seq: Key's Seq value cast failed: "++show (k,typeOf s)
             Just s' -> Right s'

  parseWireExt k@(Key i t mv)  wt raw | wt /= toWireType t =
    Left $ "parseWireExt Maybe: Key's FieldType does not match ExtField's wire type: "++show (k,toWireType t,wt)
                                        | otherwise = do
    let mkWitType :: Maybe a -> GPWitness a
        mkWitType = undefined
        witness = GPWitness `asTypeOf` (mkWitType mv)
        parsed = map (applyGet (wireGet t)) . F.toList $ raw
        errs = [ m | Left m <- parsed ]
    if null errs then Right (i,(ExtRepeated t (GPDynSeq witness (Seq.fromList [ a | Right a <- parsed ]))))
      else Left (unlines errs)

  wireGetKey k@(Key i t mv) msg = do
    let myCast :: Maybe a -> Get a
        myCast = undefined
    v <- wireGet t `asTypeOf` (myCast mv)
    let (ExtField ef) = getExtField msg
    v' <- case M.lookup i ef of
            Nothing -> return $ ExtRepeated t (GPDynSeq GPWitness (Seq.singleton v))
            Just (ExtRepeated wt (GPDynSeq GPWitness s)) ->
              case cast s of
                Nothing -> fail $ "wireGetExt Seq: previous Seq value cast failed: "++show (k,typeOf s)
                Just s' -> return (ExtRepeated wt (GPDynSeq GPWitness (s' |> v)))
            Just (ExtFromWire wt raw) ->
              case parseWireExt k wt raw of
                Left errMsg -> fail $ "wireGetExt Seq: Could not parseWireExt:\n"++errMsg
                Right (_,ExtRepeated wt (GPDynSeq GPWitness s)) ->
                  case cast s of
                    Nothing -> fail $ "wireGetExt Seq: previous Seq value cast failed: "++show (k,typeOf s)
                    Just s' -> return $ ExtRepeated wt (GPDynSeq GPWitness (s' |> v))
                wtf -> fail $ "wireGetExt Seq: Weird parseWireExt return value: "++show wtf
    let ef' = M.insert i v' ef
    seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)

-- | This is used by the generated code
wireSizeExtField (ExtField m) = F.foldl' aSize 0 (M.assocs m)  where
    aSize old (fi,(ExtFromWire wt bs)) = old +
      let tagSize = size'Varint (getWireTag (mkWireTag fi wt))
      in F.foldl' (\old new -> old + L.length new) (fromIntegral (Seq.length bs) * tagSize) bs
    aSize old (fi,(ExtOptional ft (GPDyn GPWitness d))) = old +
      let tagSize = size'Varint (getWireTag (toWireTag fi ft))
      in wireSizeReq tagSize ft d
    aSize old (fi,(ExtRepeated ft (GPDynSeq GPWitness s))) = old +
      let tagSize = size'Varint (getWireTag (toWireTag fi ft))
      in wireSizeRep tagSize ft s

-- | This is used by the generated code
wirePutExtField (ExtField m) = mapM_ aPut (M.assocs m) where
    aPut (fi,(ExtFromWire wt raw)) = F.mapM_ (\bs -> putVarUInt (getWireTag $ mkWireTag fi wt) >> putLazyByteString bs) raw
    aPut (fi,(ExtOptional ft (GPDyn GPWitness d))) = wirePutOpt (toWireTag fi ft) ft (Just d)
    aPut (fi,(ExtRepeated ft (GPDynSeq GPWitness s))) = wirePutRep (toWireTag fi ft) ft s

-- | This is used by the generated code to get messages that have extensions
getMessageExt :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
              => (FieldId -> message -> Get message)           -- handles "allowed" wireTags
              -> Get message
getMessageExt = getMessageWith extension

-- | This is used by the generated code to get messages that have extensions
getBareMessageExt :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
                  => (FieldId -> message -> Get message)           -- handles "allowed" wireTags
                  -> Get message
getBareMessageExt = getBareMessageWith extension

-- get a value from the wire into the message's ExtField
-- no validity check is performed on FieldId versus any range
extension :: ExtendMessage a => FieldId -> WireType -> a -> Get a
extension fieldId wireType msg = do
  let (ExtField ef) = getExtField msg
      badwt wt = do here <- bytesRead
                    fail $ "Conflicting wire types at byte position "++show here ++ " for extension to message: "++show (typeOf msg,fieldId,wireType,wt)
  case M.lookup fieldId ef of
    Nothing -> do
       bs <- wireGetFromWire fieldId wireType
       let v' = ExtFromWire wireType (Seq.singleton bs)
           ef' = M.insert fieldId v' ef
       seq v' $ seq ef' $ return $ putExtField (ExtField ef') msg
    Just (ExtFromWire wt raw) | wt /= wireType -> badwt wt
                              | otherwise -> do
      bs <- wireGetFromWire fieldId wireType
      let v' = ExtFromWire wt (raw |> bs)
          ef' = M.insert fieldId v' ef
      seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)
    Just (ExtOptional ft (GPDyn x@GPWitness a)) | toWireType ft /= wireType -> badwt (toWireType ft)
                                                | otherwise -> do
      b <- wireGet ft
      let v' = ExtOptional ft (GPDyn x (mergeAppend a b))
          ef' = M.insert fieldId v' ef
      seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)
    Just (ExtRepeated ft (GPDynSeq x@GPWitness s)) | toWireType ft /= wireType -> badwt (toWireType ft)
                                                   | otherwise -> do
      a <- wireGet ft
      let v' = ExtRepeated ft (GPDynSeq x (s |> a))
          ef' = M.insert fieldId v' ef
      seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)

wireGetFromWire :: FieldId -> WireType -> Get.Get ByteString
wireGetFromWire fi wt = getLazyByteString =<< calcLen where
  calcLen = case wt of
              0 -> lenOf (spanOf (>=128) >> skip 1)
              1 -> return 8
              2 -> lookAhead $ do
                     here <- Get.bytesRead
                     len <- getVarInt
                     there <- Get.bytesRead
                     return ((there-here)+len)
              3 -> lenOf skipGroup
              4 -> fail $ "Cannot wireGetFromWire with wireType of STOP_GROUP: "++show (fi,wt)
              5 -> return 4
  lenOf g = do here <- Get.bytesRead
               there <- lookAhead (g >> Get.bytesRead)
               return (there-here)
  skipGroup = do
    (fieldId,wireType) <- fmap (splitWireTag . WireTag) getVarInt
    case wireType of
      0 -> spanOf (>=128) >> skip 1 >> skipGroup
      1 -> skip 8 >> skipGroup
      2 -> getVarInt >>= skip >> skipGroup
      3 -> skipGroup >> skipGroup
      4 | fi /= fieldId -> fail $ "skipGroup failed, fieldId mismatch bewteen START_GROUP and STOP_GROUP: "++show ((fi,wt),(fieldId,wireType))
        | otherwise -> return ()
      5 -> skip 4 >> skipGroup

class MessageAPI msg a b | msg a -> b where
  -- | Access data in a message.  The first argument is always the message.  The second argument can be one of 4 categories.
  -- * The field name of a required field acts a simple retrieval of the data from the message.
  -- * The field name of an optional field will retreive the data if it is set or lookup the default value if it is not set.
  -- * The field name of a repeated field always retrieves the (possibly empty) 'Seq' of values.
  -- * A Key for an optional or repeated value will act as the field name does above, but if there is an error it will use the defaultValue for optional types and an empty sequence for repeated types.
  getVal :: msg -> a -> b

  -- | Check whether data is present in the message.
  -- * Required fields always return 'True'.
  -- * Optional fields return whether a value is present.
  -- * Repeated field return 'False' if there are no values, otherwise they return 'True'.
  -- * Keys return as optional or repeated, but checks only if the field # is present.  A badly formed Ext'Field might contain invalid information or an empty Sequence.
  isSet :: msg -> a -> Bool
  isSet _ _ = True

instance (Default msg) => MessageAPI msg (msg -> Maybe a) a where
  getVal m f | Just v <- f m = v
          | Just v <- f defaultValue = v
          | otherwise = error "Text.ProtocolBuffers.MessageAPI.get: Impossible? defaultValue was Nothing"
  isSet m f = maybe False (const True) (f m)

instance MessageAPI msg (msg -> (Seq a)) (Seq a) where
  getVal m f = f m
  isSet m f = not (Seq.null (f m))

instance (Default v) => MessageAPI msg (Key Maybe msg v) v where
  getVal m k@(Key _ _ md) = case getExt k m of
                           Right (Just v) -> v
                           _ -> fromMaybe defaultValue md
  isSet m (Key fid _ _) = let (ExtField x) = getExtField m
                          in M.member fid x

instance (Default v) => MessageAPI msg (Key Seq msg v) (Seq v) where
  getVal m k@(Key _ _ md) = case getExt k m of
                           Right s -> s
                           _ -> Seq.empty
  isSet m (Key fid _ _) = let (ExtField x) = getExtField m
                          in M.member fid x

instance MessageAPI msg (msg -> ByteString) ByteString where getVal m f = f m
instance MessageAPI msg (msg -> Utf8) Utf8 where getVal m f = f m
instance MessageAPI msg (msg -> Double) Double where getVal m f = f m
instance MessageAPI msg (msg -> Float) Float where getVal m f = f m
instance MessageAPI msg (msg -> Int32) Int32 where getVal m f = f m
instance MessageAPI msg (msg -> Int64) Int64 where getVal m f = f m
instance MessageAPI msg (msg -> Word32) Word32 where getVal m f = f m
instance MessageAPI msg (msg -> Word64) Word64 where getVal m f = f m
