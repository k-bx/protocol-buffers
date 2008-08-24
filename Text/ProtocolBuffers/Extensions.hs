module Text.ProtocolBuffers.Extensions(getExt,putExt,wireSizeExtField,wirePutExtField
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
import qualified Data.Map as M

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Default
import Text.ProtocolBuffers.WireMessage(Wire(..),Put,BinaryParser,size'Varint,toWireTag,toWireType,wireSizeReq,wirePutReq,castWord64ToDouble,castWord32ToFloat,zzDecode32,zzDecode64,runGetOnLazy,runPut,putVarUInt)

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
  typeOf2 _ = mkTyConApp (mkTyCon "Key") [typeOf1 (undefined :: c ())]

instance (Typeable1 c,Typeable msg) => Typeable1 (Key c msg) where
  typeOf1 _ = mkTyConApp (mkTyCon "Key") [typeOf1 (undefined :: c ()),typeOf (undefined :: msg)]

instance (Typeable1 c,Typeable msg,Typeable v) => Typeable (Key c msg v) where
  typeOf _ = mkTyConApp (mkTyCon "Key") [typeOf1 (undefined :: c ()),typeOf (undefined :: msg),typeOf (undefined :: v)]

instance (Typeable1 c, Typeable msg, Typeable v) => Show (Key c msg v) where
  show key@(Key fieldId fieldType maybeDefaultValue) =
    concat ["(Key (",show fieldId
           ,") (",show fieldType
           ,") (",show maybeDefaultValue
           ,") :: Key ",show (typeOf (undefined :: msg))
           ," (",show (typeOf (undefined ::c v))
           ,"))"]

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

-- | The actual data stored with the numeric FieldId key is the
-- ExtFieldValue.  If built from the wire this can only be
-- ExtFromWire.  If a Key is used then the constructor is ExtOptional
-- or ExtRepeated as specified by the canRepeat :: Bool of the Key.
--
-- The ExtFromWire stores the WireType (for sanity checks) and a Seq of
-- WireSize (the full length of the data on the wire) and WireRaw.
--
-- ExtOptional and ExtRepeated store the FieldType (for sanity checks)
-- and one or more GPDyn values storing the actual data.
--
-- ExtFieldEmpty is for mergeEmpty and defaultValue purposes
data ExtFieldValue = ExtFromWire WireType (Seq (WireSize,WireRaw))
                   | ExtOptional FieldType GPDyn
                   | ExtRepeated FieldType GPDynSeq
  deriving (Typeable,Eq,Ord,Show)

-- | The wire data can only be lightly parsed, and this is done for
-- the data below.
data WireRaw = Raw_VarInt Integer
             | Raw_Fixed64 Word64
             | Raw_Length_Delimited ByteString Int64 ByteString -- with length header, length, without length header
             | Raw_Group ByteString -- includes end tag
             | Raw_Fixed32 Word32
  deriving (Show,Eq,Ord,Typeable)

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
  putExt :: Key c msg v -> v -> msg -> msg
  getExt :: Key c msg v -> msg -> c v
  clearExt :: Key c msg v -> msg -> msg
  parseWireExt :: Key c msg v -> msg -> msg

-- | The Key and GPWitness GADTs use GPB as a shorthand for many classes
class (Mergeable a,Default a,Wire a,Show a,Typeable a,Eq a,Ord a) => GPB a 

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

wireSizeExtField (ExtField m) = foldl' aSize 0 (M.assocs m)  where
    aSize = undefined -- XXX
--    aSize b (FieldId i,(t,GPDyn GPWitness a)) = b + wireSizeReq (size'Varint i) t a

wirePutExtField (ExtField m) = mapM_ aPut (M.assocs m) where
    aPut = undefined -- XXX
--    aPut (f@(FieldId i),(t,GPDyn GPWitness a)) = wirePutReq (toWireTag f t) t a


{-
instance Show ExtField where
  showsPrec _ (ExtField m) | M.null m = ("(ExtField [])"++)
                           | otherwise = ("(ExtField ["++) . (foldr (.) ("])"++) . intersperse (',':) .  map see  . M.assocs $ m)
    where see :: (FieldId,(FieldType,GPDyn)) -> ShowS
          see (i,(t,GPDyn GPWitness d)) = shows (i,(t,d))
-}

instance Show (GPWitness a) where
  showsPrec n (GPWitness :: GPWitness a) = ("(GPWitness :: GPWitness ("++) . shows (typeOf (undefined :: a)) . (')':) . (')':)

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

toGPDyn :: (GPB a) => a -> GPDyn
toGPDyn a = GPDyn GPWitness a

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
wireSizeGPDyn f (GPDyn GPWitness a) = wireSize f a 

wirePutGPDyn :: FieldType -> GPDyn -> Put
wirePutGPDyn f (GPDyn GPWitness a) = wirePut f a 

wireGetGPDyn :: forall a get . BinaryParser get => GPWitness a -> FieldType -> get GPDyn
wireGetGPDyn (GPWitness :: GPWitness a) f = fmap (GPDyn GPWitness) (wireGet f :: get a)

getWitness :: (GPB a) => GPDyn -> Maybe (GPWitness a)
getWitness (GPDyn x@GPWitness _) = cast x

readGPDyn :: Read a => GPWitness a -> String -> GPDyn
readGPDyn x@(GPWitness :: GPWitness a) s =
  let t :: a; t = read s
  in GPDyn x t

instance ExtKey Maybe where
  putExt (Key i t _) v msg =
      let (ExtField ef) = getExtField msg
          ef' = M.insert i (ExtOptional t (toGPDyn v)) ef
      in seq ef' (putExtField (ExtField ef') msg)
  getExt k@(Key i t _) msg =
      let (ExtField ef) = getExtField msg
      in case M.lookup i ef of
           Nothing -> Nothing
           Just (ExtOptional t' (GPDyn GPWitness d)) | t/=t' -> 
             err $ "getExt Maybe: Key's FieldType does not match ExtField's: "++show (k,t')
                                                     | otherwise ->
             case cast d of
               Nothing -> err $ "getExt Maybe: Key's value cast failed: "++show (k,typeOf d)
               Just d' -> Just d'
           Just (ExtRepeated t' _) -> err $ "getKey Maybe: ExtField has repeated type: "++show (k,t')
           Just (ExtFromWire {}) -> getExt k (parseWireExt k msg)
  clearExt (Key i _ _ ) msg =
    let (ExtField ef) = getExtField msg
        ef' = M.delete i ef
    in seq ef' (putExtField (ExtField ef') msg)
  parseWireExt k@(Key i t mv) msg =
      let (ExtField ef) = getExtField msg
      in case M.lookup i ef of
           Nothing -> msg
           Just (ExtOptional t' _) | t==t' -> msg
                                   | otherwise ->
             err $ "parseWireExt Maybe: Key's FieldType does not match ExtField's: "++show (k,t')
           Just (ExtRepeated t' _) -> err $ "parseWireExt Maybe: ExtField has repeated type: "++show (k,t')
           Just (ExtFromWire w raw) | w /= toWireType t ->
             err $ "parseWireExt Maybe: Key's FieldType does not match ExtField's wire type: "++show (k,toWireType t,w)
                                    | otherwise ->
             let (wireSize,lastRaw) = case viewr raw of
                                        EmptyR -> error $ "parseWireExt Maybe: impossible ExtFromWire has empty Seq"++show (k,w)
                                        (_ :> r) -> r
                 d = case (t,lastRaw) of
                       (1,Raw_Fixed64 x)  -> GPDyn GPWitness $ castWord64ToDouble x
                       (2,Raw_Fixed32 x)  -> GPDyn GPWitness $ castWord32ToFloat x
                       (3,Raw_VarInt x)   -> GPDyn GPWitness $ (fromInteger x :: Int64)
                       (4,Raw_VarInt x)   -> GPDyn GPWitness $ (fromInteger x :: Word64)
                       (5,Raw_VarInt x)   -> GPDyn GPWitness $ (fromInteger x :: Int32)
                       (6,Raw_Fixed64 x)  -> GPDyn GPWitness $ x
                       (7,Raw_Fixed32 x)  -> GPDyn GPWitness $ x
                       (8,Raw_VarInt x)   -> GPDyn GPWitness $ if x==0 then False else True
                       (9,Raw_Length_Delimited _ _ bs) -> GPDyn GPWitness $ Utf8 bs
-- XXX todo: merge alraw data for types 10 and 11
                       (10,Raw_Group bs)  -> GPDyn GPWitness $ ((resolve $ runGetOnLazy (wireGet t) bs) `asTypeOf` (fromJust mv))
-- XXX todo: merge alraw data for types 10 and 11
                       (11,Raw_Length_Delimited bs _ _) -> GPDyn GPWitness $ ((resolve $ runGetOnLazy (wireGet t) bs) `asTypeOf` (fromJust mv))
                       (12,Raw_Length_Delimited _ _ bs) -> GPDyn GPWitness $ bs
                       (13,Raw_VarInt x)  -> GPDyn GPWitness $ (fromInteger x :: Word32)
                       (14,Raw_VarInt x)  -> GPDyn GPWitness $ (resolve $ runGetOnLazy (wireGet t) (runPut (putVarUInt x))) `asTypeOf` (fromJust mv)
                       (15,Raw_Fixed32 x) -> GPDyn GPWitness $ (fromIntegral x :: Int32)
                       (16,Raw_Fixed64 x) -> GPDyn GPWitness $ (fromIntegral x :: Int64)
                       (17,Raw_VarInt x)  -> GPDyn GPWitness $ zzDecode32 (fromInteger x)
                       (18,Raw_VarInt x)  -> GPDyn GPWitness $ zzDecode64 (fromInteger x)
                 ef' = M.insert i (ExtOptional t d) ef
             in seq ef' (putExtField (ExtField ef') msg)

resolve :: Either String (r,ByteString) -> r
resolve (Left msg) = err $ "putExtField : could not parse embedded message or group: "++msg
resolve (Right (r,_)) = r

instance ExtKey Seq where
  putExt k@(Key i t _) v msg =
    let (ExtField ef) = getExtField msg
    in case M.lookup i ef of
         Nothing -> let ef' = M.insert i (ExtRepeated t (GPDynSeq GPWitness (Seq.singleton v))) ef
                    in seq ef' (putExtField (ExtField ef') msg)
         Just (ExtRepeated t' (GPDynSeq GPWitness s)) | t'/=t ->
           err $ "putExt Seq: Key's FieldType does not match ExtField's: "++show (k,t')
                                                      | otherwise ->
           case cast s of 
             Nothing -> err $ "putExt Seq: Key's Seq cast failed: "++show (k,typeOf s)
             Just s' -> let ef' = M.insert i (ExtRepeated t (GPDynSeq GPWitness (s' |> v))) ef
                        in seq ef' (putExtField (ExtField ef') msg)
         Just (ExtOptional t' _) -> err $ "putExt Seq: Key is repeated, ExtField has optional: "++show (k,t')
         Just (ExtFromWire {}) -> putExt k v (parseWireExt k msg)
  getExt k@(Key i t _) msg =
    let (ExtField ef) = getExtField msg
    in case M.lookup i ef of
         Nothing -> mempty
         Just (ExtRepeated t' (GPDynSeq GPWitness s)) | t'/=t -> 
           err $ "getExt Seq: Key's FieldType does not match ExtField's: "++show (k,t')
                                                      | otherwise ->
           case cast s of
             Nothing -> err $ "getExt Seq: Key's Seq value cast failed: "++show (k,typeOf s)
             Just s' -> s'
         Just (ExtOptional t' _) -> err $ "getKey Seq: ExtField has optional type: "++show (k,t')
         Just (ExtFromWire {}) -> getExt k (parseWireExt k msg)
  clearExt (Key i _ _ ) msg =
    let (ExtField ef) = getExtField msg
        ef' = M.delete i ef
    in seq ef' (putExtField (ExtField ef') msg)

instance GPB Bool
instance GPB ByteString
instance GPB Utf8
instance GPB Double
instance GPB Float
instance GPB Int32
instance GPB Int64
instance GPB Word32
instance GPB Word64
{-
data Testmsg = Testmsg { name :: String
                       , child :: Maybe Testmsg
                       , e'f :: ExtField}
  deriving (Show,Typeable,Eq,Ord)

instance ExtendMessage Testmsg where
  getExtField msg = e'f msg
  putExtField x msg = msg { e'f = x }
  validExtRanges _ = [(20000,maxBound)]

instance Wire Testmsg
instance Mergeable Testmsg
instance Default Testmsg
instance GPB Testmsg

key1 :: Key Maybe Testmsg Int64
key1 = Key 1 3 Nothing

key2 :: Key Maybe Testmsg Testmsg
key2 = Key 2 11 Nothing

testKey =
  let m0 = Testmsg "hello" Nothing (ExtField M.empty)
      m1 = putExt key1 17 (m0 {name = "world"})
      m2 = putExt key2 m0 (m1 {name = "kitty"})
      m3 = m2 { child = Just m1 }
  in m3
-}