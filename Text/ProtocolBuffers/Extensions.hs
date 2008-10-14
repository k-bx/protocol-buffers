-- | The "Extensions" module contributes two main things.  The first
-- is the definition and implementation of extensible message
-- features.  This means that the 'ExtField' data type is exported but
-- its constructor is (in an ideal world) hidden.
--
-- This first part also includes the keys for the extension fields:
-- the 'Key' data type.  These are typically defined in code generated
-- by 'hprotoc' from '.proto' file definitions.
--
-- The second main part is the 'MessageAPI' class which defines
-- 'getVal' and 'isSet'.  These allow uniform access to normal and
-- extension fields for users.
--
-- Access to extension fields is strictly though keys.  There is not
-- currently any way to query or change or clear any other extension
-- field data.
--
-- This module is likely to get broken up into pieces.
module Text.ProtocolBuffers.Extensions
  ( -- * Query functions for 'Key'
    getKeyFieldId,getKeyFieldType,getKeyDefaultValue
  -- * External types and classes
  , Key(..),ExtKey(..),MessageAPI(..)
  -- * Internal types, functions, and classes
  , wireSizeExtField,wirePutExtField,getMessageExt,getBareMessageExt,loadExtension
  , GPB,ExtField(..),ExtendMessage(..),ExtFieldValue(..)
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Generics
import Data.Ix(inRange)
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import Data.Monoid(mappend)
import Data.Sequence(Seq,(|>))
import qualified Data.Sequence as Seq
import Data.Typeable

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Get as Get (Get,runGet,Result(..),bytesRead)

err :: String -> b
err msg = error $ "Text.ProtocolBuffers.Extensions error\n"++msg

-- | The 'Key' data type is used with the 'ExtKey' class to put, get,
-- and clear external fields of messages.  The 'Key' can also be used
-- with the 'MessagesAPI' to get a possibly default value and to check
-- whether a key has been set in a message.
--
-- The 'Key' type (opaque to the user) has a phantom type of Maybe
-- or Seq that corresponds to Optional or Repeated fields. And a
-- second phantom type that matches the message type it must be used
-- with.  The third type parameter corresonds to the Haskell value
-- type.
--
-- The 'Key' is a GADT that puts all the needed class instances into
-- scope.  The actual content is the 'FieldId' ( numeric key), the
-- 'FieldType' (for sanity checks), and @Maybe v@ (a non-standard
-- default value).
--
-- When code is generated all of the known keys are taken into account
-- in the deserialization from the wire.  Unknown extension fields are
-- read as a collection of raw byte sequences.  If a key is then
-- presented it will be used to parse the bytes.
-- 
-- There is no guarantee for what happens if two Keys disagree about
-- the type of a field; in particular there may be undefined values
-- and runtime errors.  The data constructor for 'Key' has to be
-- exported to the generated code, but is not exposed to the user by
-- "Text.ProtocolBuffers".
--
data Key c msg v where
  Key :: (ExtKey c,ExtendMessage msg,GPB v) => FieldId -> FieldType -> (Maybe v) -> Key c msg v

-- | This allows reflection, in this case it gives the numerical
-- 'FieldId' of the key, from 1 to 2^29-1 (excluding 19,000 through
-- 19,999).
getKeyFieldId :: Key c msg v -> FieldId
getKeyFieldId (Key fi _ _) = fi

-- | This allows reflection, in this case it gives the 'FieldType'
-- enumeration value (1 to 18) of the
-- "Text.DescriptorProtos.FieldDescriptorProto.Type" of the field.
getKeyFieldType :: Key c msg v -> FieldType
getKeyFieldType (Key _ ft _) = ft

-- | This will return the default value for a given 'Key', which is
-- set in the '.proto' file, or if unset it is the 'defaultValue' of
-- that type.
getKeyDefaultValue :: Key c msg v -> v
getKeyDefaultValue (Key _ _ md) = fromMaybe defaultValue md

instance Typeable1 c => Typeable2 (Key c) where
  typeOf2 _ = mkTyConApp (mkTyCon "Text.ProtocolBuffers.Extensions.Key") [typeOf1 (undefined :: c ())]

instance (Typeable1 c, Typeable msg, Typeable v) => Show (Key c msg v) where
  show key@(Key fieldId fieldType maybeDefaultValue) =
    concat ["(Key (",show fieldId
           ,") (",show fieldType
           ,") (",show maybeDefaultValue
           ,") :: ",show (typeOf key)
           ,")"]

-- | 'GPWitness' is an instance witness for the 'GPB' classes.  This
-- exists mainly to be a part of 'GPDyn' or 'GPDynSeq'.
data GPWitness a where GPWitness :: (GPB a) => GPWitness a
  deriving (Typeable)

-- | The 'GPDyn' is my specialization of 'Dynamic'.  It hides the type
-- with an existential but the 'GPWitness' brings the class instances
-- into scope.  This is used in 'ExtOptional' for optional fields.
data GPDyn = forall a . GPDyn !(GPWitness a) a
  deriving (Typeable)

-- | The 'GPDynSeq' is another specialization of 'Dynamic' and is used
-- in 'ExtRepeated' for repeated fields.
data GPDynSeq = forall a . GPDynSeq !(GPWitness a) !(Seq a)
  deriving (Typeable)

-- | The WireType is used to ensure the Seq is homogenous.
-- The ByteString is the unparsed input after the tag.
-- The WireSize includes all tags.
data ExtFieldValue = ExtFromWire !WireType !(Seq ByteString)
                   | ExtOptional !FieldType !GPDyn
                   | ExtRepeated !FieldType !GPDynSeq
  deriving (Typeable,Ord,Show)

data DummyMessageType deriving (Typeable)

instance ExtendMessage DummyMessageType where
  getExtField = undefined
  putExtField = undefined
  validExtRanges = undefined

-- I want a complicated comparison here to at least allow testing of
-- setting a field, writing to wire, reading back from wire, and
-- comparing.
--
-- The comparison of ExtFromWire with ExtFromWire is conservative
-- about returning True.  It is entirely possible that if both value
-- were interpreted by the same Key that their resulting values would
-- compare True.
instance Eq ExtFieldValue where
  (==) (ExtFromWire a b) (ExtFromWire a' b') = a==a' && b==b'
  (==) (ExtOptional a b) (ExtOptional a' b') = a==a' && b==b'
  (==) (ExtRepeated a b) (ExtRepeated a' b') = a==a' && b==b'
  (==) x@(ExtOptional ft (GPDyn w@GPWitness _)) (ExtFromWire wt' s') =
    let wt = toWireType ft
    in wt==wt' && (let makeKeyType :: GPWitness a -> Key Maybe DummyMessageType a
                       makeKeyType = undefined
                       key = Key 0 ft Nothing `asTypeOf` makeKeyType w
                   in case parseWireExtMaybe key wt s' of
                        Right (_,y) -> x==y
                        _ -> False)
  (==) y@(ExtFromWire {}) x@(ExtOptional {})  = x == y
  (==) x@(ExtRepeated ft (GPDynSeq w@GPWitness _)) (ExtFromWire wt' s') =
    let wt = toWireType ft
    in wt==wt' && (let makeKeyType :: GPWitness a -> Key Seq DummyMessageType a
                       makeKeyType = undefined
                       key = Key 0 ft Nothing `asTypeOf` makeKeyType w
                   in case parseWireExtSeq key wt s' of
                        Right (_,y) -> x==y
                        _ -> False)
  (==) y@(ExtFromWire {}) x@(ExtRepeated {})  = x == y
  (==) _ _ = False

-- | ExtField is a newtype'd map from the numeric FieldId key to the
-- ExtFieldValue.  This allows for the needed class instances.
newtype ExtField = ExtField (Map FieldId ExtFieldValue)
  deriving (Typeable,Eq,Ord,Show)

-- | 'ExtendMessage' abstracts the operations of storing and
-- retrieving the 'ExtField' from the message, and provides the
-- reflection needed to know the valid field numbers.
--
-- This only used internally.
class Typeable msg => ExtendMessage msg where
  getExtField :: msg -> ExtField
  putExtField :: ExtField -> msg -> msg
  validExtRanges :: msg -> [(FieldId,FieldId)]

-- | The 'ExtKey' class has three functions for user of the API:
-- 'putExt', 'getExt', and 'clearExt'.  The 'wireGetKey' is used in
-- generated code.
--
-- There are two instances of this class, 'Maybe' for optional message
-- fields and 'Seq' for repeated message fields.  This class allows
-- for uniform treatment of these two kinds of extension fields.
class ExtKey c where
  -- | Change or clear the value of a key in a message. Passing
  -- 'Nothing' with an optional key or an empty 'Seq' with a repeated
  -- key clears the value.  This function thus maintains the invariant
  -- that having a field number in the 'ExtField' map means that the
  -- field is set and not empty.
  --
  -- This should be only way to set the contents of a extension field.
  putExt :: Key c msg v -> c v -> msg -> msg
  -- | Access the key in the message.  Optional have type @(Key Maybe
  -- msg v)@ and return type @(Maybe v)@ while repeated fields have
  -- type @(Key Seq msg v)@ and return type @(Seq v)@.
  --
  -- There are a few sources of errors with the lookup of the key:
  --
  --  * It may find unparsed bytes from loading the message. 'getExt'
  --  will attempt to parse the bytes as the key\'s value type, and
  --  may fail.  The parsing is done with the 'parseWireExt' method
  --  (which is not exported to user API).
  --
  --  * The wrong optional-key versus repeated-key type is a failure
  -- 
  --  * The wrong type of the value might be found in the map and
  --  * cause a failure
  --
  -- The failures above should only happen if two different keys are
  -- used with the same field number.
  getExt :: Key c msg v -> msg -> Either String (c v)
  -- 'clearExt' unsets the field of the 'Key' if it is present.
  clearExt :: Key c msg v -> msg -> msg
  -- 'wireGetKey' is used in generated code to load extension fields
  -- which are defined in the same '.proto' file as the message.  This
  -- results in the storing the parsed type instead of the raw bytes
  -- inside the message.
  wireGetKey :: Key c msg v -> msg -> Get msg

-- | The 'Key' and 'GPWitness' GADTs use 'GPB' as a shorthand for many
-- classes.
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

mergeExtFieldValue :: ExtFieldValue -> ExtFieldValue -> ExtFieldValue
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

mergeExtFieldValue a b = err $ "mergeExtFieldValue : mismatch of constructors "++show (a,b)

instance Default ExtField where
  defaultValue = ExtField M.empty

instance Show (GPWitness a) where
  showsPrec _n GPWitness = ("(GPWitness :: GPWitness ("++) . shows (typeOf (undefined :: a)) . (')':) . (')':)

instance Eq (GPWitness a) where
  (==) GPWitness GPWitness = True
  (/=) GPWitness GPWitness = False

instance Ord (GPWitness a) where
  compare GPWitness GPWitness = EQ

instance (GPB a) => Data (GPWitness a) where
  gunfold _k z c = case constrIndex c of
                     1 -> z GPWitness
                     _ -> err "gunfold of GPWitness error"
  toConstr GPWitness = gpWitnessC
  dataTypeOf _ = gpWitnessDT

gpWitnessC :: Constr
gpWitnessC = mkConstr gpWitnessDT "GPWitness" [] Prefix 
gpWitnessDT :: DataType
gpWitnessDT = mkDataType "GPWitness" [gpWitnessC]

{-
gpDynC :: Constr
gpDynC = mkConstr gpDynDT "GPDyn" ["a"] Prefix
gpDynDT :: DataType
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
-}

instance Eq GPDyn where
  (==) a b = fromMaybe False (eqGPDyn a b)

instance Ord GPDyn where
  compare a b = fromMaybe (compare (show a) (show b)) (ordGPDyn a b)

instance Show GPDyn where
  showsPrec _n (GPDyn x@GPWitness a) = ("(GPDyn "++) . shows x . (" ("++) . shows a . ("))"++)

instance Eq GPDynSeq where
  (==) a b = fromMaybe False (eqGPDynSeq a b)

instance Ord GPDynSeq where
  compare a b = fromMaybe (compare (show a) (show b)) (ordGPDynSeq a b)

instance Show GPDynSeq where
  showsPrec _n (GPDynSeq x@GPWitness s) = ("(GPDynSeq "++) . shows x . (" ("++) . shows s . ("))"++)

ordGPDyn :: GPDyn -> GPDyn -> Maybe Ordering
ordGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (compare a1) (cast a2)

eqGPDyn :: GPDyn -> GPDyn -> Maybe Bool
eqGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (a1==) (cast a2)

-- showGPDyn :: GPDyn -> String
-- showGPDyn (GPDyn GPWitness s) = show s

ordGPDynSeq :: GPDynSeq -> GPDynSeq -> Maybe Ordering
ordGPDynSeq (GPDynSeq GPWitness a1) (GPDynSeq GPWitness a2) = fmap (compare a1) (cast a2)

eqGPDynSeq :: GPDynSeq -> GPDynSeq -> Maybe Bool
eqGPDynSeq (GPDynSeq GPWitness a1) (GPDynSeq GPWitness a2) = fmap (a1==) (cast a2)

-- showGPDynSeq :: GPDynSeq -> String
-- showGPDynSeq (GPDynSeq GPWitness s) = show s

-- wireSizeGPDyn :: FieldType -> GPDyn -> WireSize
-- wireSizeGPDyn ft (GPDyn GPWitness a) = wireSize ft a 

-- wirePutGPDyn :: FieldType -> GPDyn -> Put
-- wirePutGPDyn ft (GPDyn GPWitness a) = wirePut ft a 

-- wireGetGPDyn :: forall a. GPWitness a -> FieldType -> Get GPDyn
-- wireGetGPDyn GPWitness ft = fmap (GPDyn GPWitness) (wireGet ft :: Get a)

-- getWitness :: (GPB a) => GPDyn -> Maybe (GPWitness a)
-- getWitness (GPDyn x@GPWitness _) = cast x

-- readGPDyn :: forall a . Read a => GPWitness a -> String -> GPDyn
-- readGPDyn x@(GPWitness) s =
--   let t :: a; t = read s
--   in GPDyn x t

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
         Just (ExtFromWire wt raw) -> either Left (getExt' . snd) (parseWireExtMaybe k wt raw)
         Just x -> getExt' x
   where getExt' (ExtRepeated t' _) = Left $ "getKey Maybe: ExtField has repeated type: "++show (k,t')
         getExt' (ExtOptional t' (GPDyn GPWitness d)) | t/=t' =
           Left $ "getExt Maybe: Key's FieldType does not match ExtField's: "++show (k,t')
                                                      | otherwise =
           case cast d of
             Nothing -> Left $ "getExt Maybe: Key's value cast failed: "++show (k,typeOf d)
             Just d' -> Right (Just d')
         getExt' _ = err $ "Impossible? getExt.getExt' Maybe should not have this case (after parseWireExt)!"
  wireGetKey k@(Key i t mv) msg = do
    let myCast :: Maybe a -> Get a
        myCast = undefined
    v <- wireGet t `asTypeOf` (myCast mv)
    let (ExtField ef) = getExtField msg
    v' <- case M.lookup i ef of
            Nothing -> return $ ExtOptional t (GPDyn GPWitness v)
            Just (ExtOptional t' (GPDyn GPWitness vOld)) | t /= t' ->
              fail $ "wireGetKey Maybe: Key mismatch! found wrong field type: "++show (k,t,t')
                                                         | otherwise ->
              case cast vOld of
                Nothing -> fail $ "wireGetKey Maybe: previous Maybe value case failed: "++show (k,typeOf vOld)
                Just vOld' -> return $ ExtOptional t (GPDyn GPWitness (mergeAppend vOld' v))
            Just (ExtFromWire wt raw) ->
              case parseWireExtMaybe k wt raw of
                Left errMsg -> fail $ "wireGetKey Maybe: Could not parseWireExtMaybe: "++show k++"\n"++errMsg
                Right (_,ExtOptional t' (GPDyn GPWitness vOld)) | t/=t' ->
                  fail $ "wireGetKey Maybe: Key mismatch! found wrong field type: "++show (k,t,t')
                                                         | otherwise ->
                  case cast vOld of
                    Nothing -> fail $ "wireGetKey Maybe: previous Maybe value case failed: "++show (k,typeOf vOld)
                    Just vOld' -> return $ ExtOptional t (GPDyn GPWitness (mergeAppend vOld' v))
                wtf -> fail $ "wireGetKey Maybe: Weird parseGetWireMaybe return value: "++show (k,wtf)
            wtf -> fail $ "wireGetKey Maybe: ExtRepeated found with ExtOptional expected: "++show (k,wtf)
    let ef' = M.insert i v' ef
    seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)

-- | used by 'getVal' and 'wireGetKey' for the 'Maybe' instance
parseWireExtMaybe :: Key Maybe msg v -> WireType -> Seq ByteString -> Either String (FieldId,ExtFieldValue)
parseWireExtMaybe k@(Key fi ft mv)  wt raw | wt /= toWireType ft =
  Left $ "parseWireExt Maybe: Key's FieldType does not match ExtField's wire type: "++show (k,toWireType ft,wt)
                                      | otherwise = do
  let mkWitType :: Maybe a -> GPWitness a
      mkWitType = undefined
      witness = GPWitness `asTypeOf` (mkWitType mv)
      parsed = map (applyGet (wireGet ft)) . F.toList $ raw
      errs = [ m | Left m <- parsed ]
  if null errs then Right (fi,(ExtOptional ft (GPDyn witness (mergeConcat [ a | Right a <- parsed ]))))
    else Left (unlines errs)

-- | Converts the the 'Result' into an 'Either' type and enforces
-- consumption of entire 'ByteString'.  Used by 'parseWireExtMaybe'
-- and 'parseWireExtSeq' to process raw wire input that has been
-- stored in an 'ExtField'.
applyGet :: Get r -> ByteString -> Either String r
applyGet g bsIn = resolveEOF (runGet g bsIn) where
  resolveEOF :: Result r -> Either String r
  resolveEOF (Failed i s) = Left ("Failed at "++show i++" : "++s)
  resolveEOF (Finished bs _i r) | L.null bs = Right r
                                | otherwise = Left "Not all input consumed"
  resolveEOF (Partial {}) = Left "Not enough input"

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
         Just (ExtFromWire wt raw) -> either Left (getExt' . snd) (parseWireExtSeq k wt raw)
         Just x -> getExt' x
   where getExt' (ExtOptional t' _) = Left $ "getKey Seq: ExtField has optional type: "++show (k,t')
         getExt' (ExtRepeated t' (GPDynSeq GPWitness s)) | t'/=t =
           Left $ "getExt Seq: Key's FieldType does not match ExtField's: "++show (k,t')
                                                         | otherwise =
           case cast s of
             Nothing -> Left $ "getExt Seq: Key's Seq value cast failed: "++show (k,typeOf s)
             Just s' -> Right s'
         getExt' _ = err $ "Impossible? getExt.getExt' Maybe should not have this case (after parseWireExtSeq)!"
             
  -- This is more complicated than the Maybe instance because the old
  -- Seq needs to be retrieved and perhaps parsed and then appended
  -- to.  All sanity checks are included below.  TODO: do enough
  -- testing to be confident in removing some checks.
  wireGetKey k@(Key i t mv) msg = do
    let myCast :: Maybe a -> Get a
        myCast = undefined
    v <- wireGet t `asTypeOf` (myCast mv)
    let (ExtField ef) = getExtField msg
    v' <- case M.lookup i ef of
            Nothing -> return $ ExtRepeated t (GPDynSeq GPWitness (Seq.singleton v))
            Just (ExtRepeated t' (GPDynSeq GPWitness s)) | t/=t' ->
              fail $ "wireGetKey Seq: Key mismatch! found wrong field type: "++show (k,t,t')
                                                         | otherwise ->
              case cast s of
                Nothing -> fail $ "wireGetKey Seq: previous Seq value cast failed: "++show (k,typeOf s)
                Just s' -> return $ ExtRepeated t (GPDynSeq GPWitness (s' |> v))
            Just (ExtFromWire wt raw) ->
              case parseWireExtSeq k wt raw of
                Left errMsg -> fail $ "wireGetKey Seq: Could not parseWireExtSeq: "++show k++"\n"++errMsg
                Right (_,ExtRepeated t' (GPDynSeq GPWitness s)) | t/=t' ->
                  fail $ "wireGetKey Seq: Key mismatch! parseWireExtSeq returned wrong field type: "++show (k,t,t')
                                                                | otherwise ->
                  case cast s of
                    Nothing -> fail $ "wireGetKey Seq: previous Seq value cast failed: "++show (k,typeOf s)
                    Just s' -> return $ ExtRepeated t (GPDynSeq GPWitness (s' |> v))
                wtf -> fail $ "wireGetKey Seq: Weird parseWireExtSeq return value: "++show (k,wtf)
            wtf -> fail $ "wireGetKey Seq: ExtOptional found when ExtRepeated expected: "++show (k,wtf)
    let ef' = M.insert i v' ef
    seq v' $ seq ef' $ return (putExtField (ExtField ef') msg)

-- | used by 'getVal' and 'wireGetKey' for the 'Seq' instance
parseWireExtSeq :: Key Seq msg v -> WireType -> Seq ByteString -> Either String (FieldId,ExtFieldValue)
parseWireExtSeq k@(Key i t mv)  wt raw | wt /= toWireType t =
  Left $ "parseWireExt Maybe: Key mismatch! Key's FieldType does not match ExtField's wire type: "++show (k,toWireType t,wt)
                                      | otherwise = do
  let mkWitType :: Maybe a -> GPWitness a
      mkWitType = undefined
      witness = GPWitness `asTypeOf` (mkWitType mv)
      parsed = map (applyGet (wireGet t)) . F.toList $ raw
      errs = [ m | Left m <- parsed ]
  if null errs then Right (i,(ExtRepeated t (GPDynSeq witness (Seq.fromList [ a | Right a <- parsed ]))))
    else Left (unlines errs)

-- | This is used by the generated code
wireSizeExtField :: ExtField -> WireSize
wireSizeExtField (ExtField m) = F.foldl' aSize 0 (M.assocs m)  where
  aSize old (fi,(ExtFromWire wt raw)) = old +
    let tagSize = size'Varint (getWireTag (mkWireTag fi wt))
    in F.foldl' (\oldVal new -> oldVal + L.length new) (fromIntegral (Seq.length raw) * tagSize) raw
  aSize old (fi,(ExtOptional ft (GPDyn GPWitness d))) = old +
    let tagSize = size'Varint (getWireTag (toWireTag fi ft))
    in wireSizeReq tagSize ft d
  aSize old (fi,(ExtRepeated ft (GPDynSeq GPWitness s))) = old +
    let tagSize = size'Varint (getWireTag (toWireTag fi ft))
    in wireSizeRep tagSize ft s

-- | This is used by the generated code. The data is serialized in
-- order of increasing field number.
wirePutExtField :: ExtField -> Put
wirePutExtField (ExtField m) = mapM_ aPut (M.assocs m) where
  aPut (fi,(ExtFromWire wt raw)) = F.mapM_ (\bs -> putVarUInt (getWireTag $ mkWireTag fi wt) >> putLazyByteString bs) raw
  aPut (fi,(ExtOptional ft (GPDyn GPWitness d))) = wirePutOpt (toWireTag fi ft) ft (Just d)
  aPut (fi,(ExtRepeated ft (GPDynSeq GPWitness s))) = wirePutRep (toWireTag fi ft) ft s

-- | This is used by the generated code to get messages that have extensions
getMessageExt :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
              => (FieldId -> message -> Get message)           -- handles "allowed" wireTags
              -> Get message
getMessageExt = getMessageWith loadExtension

-- | This is used by the generated code to get messages that have extensions
getBareMessageExt :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
                  => (FieldId -> message -> Get message)           -- handles "allowed" wireTags
                  -> Get message
getBareMessageExt = getBareMessageWith loadExtension

-- | 'isValidExt' is used by 'extension' to check whether the field
-- number is in one of the ranges declared in the '.proto' file.
{-# INLINE isValidExt #-}
isValidExt ::  ExtendMessage a => FieldId -> a -> Bool
isValidExt fi msg = any (flip inRange fi) (validExtRanges msg)

-- | get a value from the wire into the message's ExtField. This is
-- used by 'getMessageExt' and 'getBareMessageExt' above.
loadExtension :: (ReflectDescriptor a, ExtendMessage a) => FieldId -> WireType -> a -> Get a
loadExtension fieldId wireType msg | isValidExt fieldId msg = do
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
loadExtension fieldId wireType msg = unknown fieldId wireType msg

class MessageAPI msg a b | msg a -> b where
  -- | Access data in a message.  The first argument is always the
  -- message.  The second argument can be one of 4 categories.
  --
  -- * The field name of a required field acts a simple retrieval of
  -- the data from the message.
  --
  -- * The field name of an optional field will retreive the data if
  -- it is set or lookup the default value if it is not set.
  --
  -- * The field name of a repeated field always retrieves the
  -- (possibly empty) 'Seq' of values.
  --
  -- * A Key for an optional or repeated value will act as the field
  -- name does above, but if there is a type mismatch or parse error
  -- it will use the defaultValue for optional types and an empty
  -- sequence for repeated types.
  getVal :: msg -> a -> b

  -- | Check whether data is present in the message.
  --
  -- * Required fields always return 'True'.
  --
  -- * Optional fields return whether a value is present.
  --
  -- * Repeated field return 'False' if there are no values, otherwise
  -- they return 'True'.
  --
  -- * Keys return as optional or repeated, but checks only if the
  -- field # is present.  This assumes that there are no collisions
  -- where more that one key refers to the same field number of this
  -- message type.
  isSet :: msg -> a -> Bool
  isSet _ _ = True

instance (Default msg,Default a) => MessageAPI msg (msg -> Maybe a) a where
  getVal m f = fromMaybe (fromMaybe defaultValue (f defaultValue)) (f m)
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
  getVal m k@(Key _ _ _) = case getExt k m of
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
