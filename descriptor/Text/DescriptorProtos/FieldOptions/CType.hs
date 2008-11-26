module Text.DescriptorProtos.FieldOptions.CType (CType(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data CType = CORD
           | STRING_PIECE
           deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable CType
 
instance P'.Bounded CType where
  minBound = CORD
  maxBound = STRING_PIECE
 
instance P'.Default CType where
  defaultValue = CORD
 
toMaybe'Enum :: P'.Int -> P'.Maybe CType
toMaybe'Enum 1 = P'.Just CORD
toMaybe'Enum 2 = P'.Just STRING_PIECE
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum CType where
  fromEnum (CORD) = 1
  fromEnum (STRING_PIECE) = 2
  toEnum
   = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FieldOptions.CType") .
      toMaybe'Enum
  succ (CORD) = STRING_PIECE
  succ _ = P'.error "hprotoc generated code: succ failure for type Text.DescriptorProtos.FieldOptions.CType"
  pred (STRING_PIECE) = CORD
  pred _ = P'.error "hprotoc generated code: pred failure for type Text.DescriptorProtos.FieldOptions.CType"
 
instance P'.Wire CType where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB CType
 
instance P'.MessageAPI msg' (msg' -> CType) CType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum CType where
  reflectEnum = [(1, "CORD", CORD), (2, "STRING_PIECE", STRING_PIECE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".google.protobuf.FieldOptions.CType") ["Text"] ["DescriptorProtos", "FieldOptions"] "CType")
      ["Text", "DescriptorProtos", "FieldOptions", "CType.hs"]
      [(1, "CORD"), (2, "STRING_PIECE")]