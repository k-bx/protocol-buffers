module UnittestProto.TestSparseEnum (TestSparseEnum(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestSparseEnum = SPARSE_A
                    | SPARSE_B
                    | SPARSE_C
                    | SPARSE_D
                    | SPARSE_E
                    | SPARSE_F
                    | SPARSE_G
                    deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestSparseEnum
 
instance P'.Bounded TestSparseEnum where
  minBound = SPARSE_A
  maxBound = SPARSE_G
 
instance P'.Default TestSparseEnum where
  defaultValue = SPARSE_A
 
instance P'.Enum TestSparseEnum where
  fromEnum (SPARSE_A) = 123
  fromEnum (SPARSE_B) = 62374
  fromEnum (SPARSE_C) = 12589234
  fromEnum (SPARSE_D) = 15
  fromEnum (SPARSE_E) = 53452
  fromEnum (SPARSE_F) = 0
  fromEnum (SPARSE_G) = 2
  toEnum 123 = SPARSE_A
  toEnum 62374 = SPARSE_B
  toEnum 12589234 = SPARSE_C
  toEnum 15 = SPARSE_D
  toEnum 53452 = SPARSE_E
  toEnum 0 = SPARSE_F
  toEnum 2 = SPARSE_G
  succ (SPARSE_A) = SPARSE_B
  succ (SPARSE_B) = SPARSE_C
  succ (SPARSE_C) = SPARSE_D
  succ (SPARSE_D) = SPARSE_E
  succ (SPARSE_E) = SPARSE_F
  succ (SPARSE_F) = SPARSE_G
  pred (SPARSE_B) = SPARSE_A
  pred (SPARSE_C) = SPARSE_B
  pred (SPARSE_D) = SPARSE_C
  pred (SPARSE_E) = SPARSE_D
  pred (SPARSE_F) = SPARSE_E
  pred (SPARSE_G) = SPARSE_F
 
instance P'.Wire TestSparseEnum where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB TestSparseEnum
 
instance P'.MessageAPI msg' (msg' -> TestSparseEnum) TestSparseEnum where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum TestSparseEnum where
  reflectEnum
    = [(123, "SPARSE_A", SPARSE_A), (62374, "SPARSE_B", SPARSE_B), (12589234, "SPARSE_C", SPARSE_C), (15, "SPARSE_D", SPARSE_D),
       (53452, "SPARSE_E", SPARSE_E), (0, "SPARSE_F", SPARSE_F), (2, "SPARSE_G", SPARSE_G)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName "" "UnittestProto" "TestSparseEnum") ["UnittestProto", "TestSparseEnum.hs"]
        [(123, "SPARSE_A"), (62374, "SPARSE_B"), (12589234, "SPARSE_C"), (15, "SPARSE_D"), (53452, "SPARSE_E"), (0, "SPARSE_F"),
         (2, "SPARSE_G")]