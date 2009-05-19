module Text.DescriptorProtos.FieldDescriptorProto.Label (Label(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
           deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Label
 
instance P'.Bounded Label where
  minBound = LABEL_OPTIONAL
  maxBound = LABEL_REPEATED
 
instance P'.Default Label where
  defaultValue = LABEL_OPTIONAL
 
toMaybe'Enum :: P'.Int -> P'.Maybe Label
toMaybe'Enum 1 = P'.Just LABEL_OPTIONAL
toMaybe'Enum 2 = P'.Just LABEL_REQUIRED
toMaybe'Enum 3 = P'.Just LABEL_REPEATED
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum Label where
  fromEnum (LABEL_OPTIONAL) = 1
  fromEnum (LABEL_REQUIRED) = 2
  fromEnum (LABEL_REPEATED) = 3
  toEnum
   = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FieldDescriptorProto.Label") .
      toMaybe'Enum
  succ (LABEL_OPTIONAL) = LABEL_REQUIRED
  succ (LABEL_REQUIRED) = LABEL_REPEATED
  succ _ = P'.error "hprotoc generated code: succ failure for type Text.DescriptorProtos.FieldDescriptorProto.Label"
  pred (LABEL_REQUIRED) = LABEL_OPTIONAL
  pred (LABEL_REPEATED) = LABEL_REQUIRED
  pred _ = P'.error "hprotoc generated code: pred failure for type Text.DescriptorProtos.FieldDescriptorProto.Label"
 
instance P'.Wire Label where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Label
 
instance P'.MessageAPI msg' (msg' -> Label) Label where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Label where
  reflectEnum
   = [(1, "LABEL_OPTIONAL", LABEL_OPTIONAL), (2, "LABEL_REQUIRED", LABEL_REQUIRED), (3, "LABEL_REPEATED", LABEL_REPEATED)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".google.protobuf.FieldDescriptorProto.Label") ["Text"] ["DescriptorProtos", "FieldDescriptorProto"]
        "Label")
      ["Text", "DescriptorProtos", "FieldDescriptorProto", "Label.hs"]
      [(1, "LABEL_OPTIONAL"), (2, "LABEL_REQUIRED"), (3, "LABEL_REPEATED")]