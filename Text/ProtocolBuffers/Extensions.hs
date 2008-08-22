module Text.ProtocolBuffers.Extensions(getExt,putExt,wireSizeExtField,wirePutExtField
                                      ,Key(..),ExtField(..),ExtendMessage(..)
                                      ) where

import Data.Map(Map)
import Data.Dynamic
import Data.Generics
import Data.List(intersperse,foldl')
import Data.Maybe(catMaybes,fromMaybe)
import Data.Typeable
import qualified Data.Map as M

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.WireMessage(Wire(..),Put,BinaryParser,size'Varint,toWireTag,wireSizeReq,wirePutReq)
import Text.ProtocolBuffers.Mergeable(Mergeable(..))
import Text.ProtocolBuffers.Default(Default(..))

data Key msg v where
  Key :: (ExtendMessage msg,GPB v) => FieldId -> FieldType -> Key msg v
 deriving (Typeable)

newtype ExtField = ExtField (Map FieldId (FieldType,GPDyn))
  deriving (Mergeable,Default,Typeable,Eq,Ord,Show)

instance (Typeable msg, Typeable v) => Show (Key msg v) where
  show k@(Key i t) = '(':concat ["Key (",show i,") (",show t,") :: ",show (typeOf k),")"]

instance (Ord k,Mergeable v) => Mergeable (Map k v) where
  mergeEmpty = M.empty
  mergeAppend = M.unionWith mergeAppend

instance (Ord k,Mergeable v) => Default (Map k v) where
  defaultValue = M.empty

instance Mergeable (FieldType,GPDyn) where
  mergeEmpty = (error "mergeEmpty for (FieldType,GPDyn) has undefined FieldType",mergeEmpty)
  mergeAppend (_,a) (t,b) = (t,mergeAppend a b)

wireSizeExtField (ExtField m) = foldl' aSize 0 (M.assocs m)  where
    aSize b (FieldId i,(t,GPDyn GPWitness a)) = b + wireSizeReq (size'Varint i) t a
wirePutExtField (ExtField m) = mapM_ aPut (M.assocs m) where
    aPut (f@(FieldId i),(t,GPDyn GPWitness a)) = wirePutReq (toWireTag f t) t a

{-
instance Show ExtField where
  showsPrec _ (ExtField m) | M.null m = ("(ExtField [])"++)
                           | otherwise = ("(ExtField ["++) . (foldr (.) ("])"++) . intersperse (',':) .  map see  . M.assocs $ m)
    where see :: (FieldId,(FieldType,GPDyn)) -> ShowS
          see (i,(t,GPDyn GPWitness d)) = shows (i,(t,d))
-}
data GPWitness a where GPWitness :: (GPB a) => GPWitness a
  deriving Typeable

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

data GPDyn = forall a . GPDyn (GPWitness a) a
  deriving (Typeable)

instance Show GPDyn where
  showsPrec n (GPDyn x@GPWitness a) = ("(GPDyn "++) . shows x . (" ("++) . shows a . ("))"++)

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

instance Mergeable GPDyn where
  mergeEmpty = GPDyn GPWitness (0::Int32)
  mergeAppend a b = fromMaybe b (mergeAppendGPDyn a b)

wireSizeGPDyn :: FieldType -> GPDyn -> WireSize
wireSizeGPDyn f (GPDyn GPWitness a) = wireSize f a 

wirePutGPDyn :: FieldType -> GPDyn -> Put
wirePutGPDyn f (GPDyn GPWitness a) = wirePut f a 

wireGetGPDyn :: forall a get . BinaryParser get => GPWitness a -> FieldType -> get GPDyn
wireGetGPDyn (GPWitness :: GPWitness a) f = fmap (GPDyn GPWitness) (wireGet f :: get a)

instance Eq GPDyn where
  (==) a b = fromMaybe False (eqGPDyn a b)

instance Ord GPDyn where
  compare a b = fromMaybe (compare (show a) (show b)) (ordGPDyn a b)

getWitness :: (Show a, Typeable a, Default a, Mergeable a, Wire a, Eq a, Ord a) => GPDyn -> Maybe (GPWitness a)
getWitness (GPDyn x@GPWitness _) = cast x

ordGPDyn :: GPDyn -> GPDyn -> Maybe Ordering
ordGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (compare a1) (cast a2)

eqGPDyn :: GPDyn -> GPDyn -> Maybe Bool
eqGPDyn (GPDyn GPWitness a1) (GPDyn GPWitness a2) = fmap (a1==) (cast a2)

showGPDyn :: GPDyn -> String
showGPDyn (GPDyn GPWitness s) = show s

readGPDyn :: Read a => GPWitness a -> String -> GPDyn
readGPDyn x@(GPWitness :: GPWitness a) s =
  let t :: a; t = read s
  in GPDyn x t

data MsgHolder where
   MsgHolder :: (Show msg,Mergeable msg,Default msg,Wire msg,Eq msg) => msg -> Dynamic -> MsgHolder
 deriving (Typeable)

class ExtendMessage msg where
  getExtField :: msg -> ExtField
  putExtField :: ExtField -> msg -> msg
  validExtRanges :: msg -> [(FieldId,FieldId)]

putExt :: (GPB v) => Key msg v -> v-> msg -> msg
putExt (Key i t) v msg =
  let (ExtField ef) = getExtField msg
      ef' = M.insert i (t,toGPDyn v) ef
  in seq ef' (putExtField (ExtField ef') msg)

getExt :: Key msg v -> msg -> Maybe v
getExt (Key i t) msg =
  let (ExtField ef) = getExtField msg
  in case M.lookup i ef of
       Nothing -> Nothing
       Just (t',GPDyn GPWitness d) | t'/=t -> Nothing
                                   | otherwise -> cast d

data Testmsg = Testmsg {name :: String
                       ,child :: (Maybe Testmsg)
                       ,e'f :: ExtField}
  deriving (Show,Typeable,Eq,Ord)

instance ExtendMessage Testmsg where
  getExtField msg = e'f msg
  putExtField x msg = msg { e'f = x }
  validExtRanges _ = [(20000,maxBound)]

instance Wire Testmsg
instance Mergeable Testmsg
instance Default Testmsg
instance GPB Testmsg

key1 :: Key Testmsg Int64
key1 = Key 1 3

key2 :: Key Testmsg Testmsg
key2 = Key 2 11

testKey =
  let m0 = Testmsg "hello" Nothing (ExtField M.empty)
      m1 = putExt key1 17 (m0 {name = "world"})
      m2 = putExt key2 m0 (m1 {name = "kitty"})
      m3 = m2 { child = Just m1 }
  in m3

instance GPB Bool
instance GPB ByteString
instance GPB Utf8
instance GPB Double
instance GPB Float
instance GPB Int32
instance GPB Int64
instance GPB Word32
instance GPB Word64
