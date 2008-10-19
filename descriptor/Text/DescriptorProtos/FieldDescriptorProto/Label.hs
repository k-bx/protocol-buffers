module Text.DescriptorProtos.FieldDescriptorProto.Label (Label(..)) where
import Prelude ((+))
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
 
instance P'.Enum Label where
  fromEnum (LABEL_OPTIONAL) = 1
  fromEnum (LABEL_REQUIRED) = 2
  fromEnum (LABEL_REPEATED) = 3
  toEnum 1 = LABEL_OPTIONAL
  toEnum 2 = LABEL_REQUIRED
  toEnum 3 = LABEL_REPEATED
  succ (LABEL_OPTIONAL) = LABEL_REQUIRED
  succ (LABEL_REQUIRED) = LABEL_REPEATED
  pred (LABEL_REQUIRED) = LABEL_OPTIONAL
  pred (LABEL_REPEATED) = LABEL_REQUIRED
 
instance P'.Wire Label where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB Label
 
instance P'.MessageAPI msg' (msg' -> Label) Label where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Label where
  reflectEnum
    = [(1, "LABEL_OPTIONAL", LABEL_OPTIONAL), (2, "LABEL_REQUIRED", LABEL_REQUIRED), (3, "LABEL_REPEATED", LABEL_REPEATED)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName (P'.Utf8 (P'.pack "MakeReflections.xxx")) "Text" "DescriptorProtos.FieldDescriptorProto" "Label")
        ["Text", "DescriptorProtos", "FieldDescriptorProto", "Label.hs"]
        [(1, "LABEL_OPTIONAL"), (2, "LABEL_REQUIRED"), (3, "LABEL_REPEATED")]