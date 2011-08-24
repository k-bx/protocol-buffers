{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.DescriptorProtos.FieldOptions.CType (CType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data CType = STRING
           | CORD
           | STRING_PIECE
           deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable CType
 
instance Prelude'.Bounded CType where
  minBound = STRING
  maxBound = STRING_PIECE
 
instance P'.Default CType where
  defaultValue = STRING
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe CType
toMaybe'Enum 0 = Prelude'.Just STRING
toMaybe'Enum 1 = Prelude'.Just CORD
toMaybe'Enum 2 = Prelude'.Just STRING_PIECE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum CType where
  fromEnum STRING = 0
  fromEnum CORD = 1
  fromEnum STRING_PIECE = 2
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FieldOptions.CType") .
      toMaybe'Enum
  succ STRING = CORD
  succ CORD = STRING_PIECE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Text.DescriptorProtos.FieldOptions.CType"
  pred CORD = STRING
  pred STRING_PIECE = CORD
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Text.DescriptorProtos.FieldOptions.CType"
 
instance P'.Wire CType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB CType
 
instance P'.MessageAPI msg' (msg' -> CType) CType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum CType where
  reflectEnum = [(0, "STRING", STRING), (1, "CORD", CORD), (2, "STRING_PIECE", STRING_PIECE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".google.protobuf.FieldOptions.CType") ["Text"] ["DescriptorProtos", "FieldOptions"] "CType")
      ["Text", "DescriptorProtos", "FieldOptions", "CType.hs"]
      [(0, "STRING"), (1, "CORD"), (2, "STRING_PIECE")]