module Text.DescriptorProtos.FieldOptions.CType (CType(..)) where
import Prelude ((+))
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
 
instance P'.Enum CType where
  fromEnum (CORD) = 1
  fromEnum (STRING_PIECE) = 2
  toEnum 1 = CORD
  toEnum 2 = STRING_PIECE
  succ (CORD) = STRING_PIECE
  pred (STRING_PIECE) = CORD
 
instance P'.Wire CType where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB CType
 
instance P'.MessageAPI msg' (msg' -> CType) CType where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum CType where
  reflectEnum = [(1, "CORD", CORD), (2, "STRING_PIECE", STRING_PIECE)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName "Text" "DescriptorProtos.FieldOptions" "CType")
        ["Text", "DescriptorProtos", "FieldOptions", "CType.hs"]
        [(1, "CORD"), (2, "STRING_PIECE")]