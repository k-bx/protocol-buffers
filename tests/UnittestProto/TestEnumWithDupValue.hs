module UnittestProto.TestEnumWithDupValue (TestEnumWithDupValue(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestEnumWithDupValue = FOO1
                          | BAR1
                          | BAZ
                          | FOO2
                          | BAR2
                          deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestEnumWithDupValue
 
instance P'.Bounded TestEnumWithDupValue where
  minBound = FOO1
  maxBound = BAR2
 
instance P'.Default TestEnumWithDupValue where
  defaultValue = FOO1
 
instance P'.Enum TestEnumWithDupValue where
  fromEnum (FOO1) = 1
  fromEnum (BAR1) = 2
  fromEnum (BAZ) = 3
  fromEnum (FOO2) = 1
  fromEnum (BAR2) = 2
  toEnum 1 = FOO1
  toEnum 2 = BAR1
  toEnum 3 = BAZ
  toEnum 1 = FOO2
  toEnum 2 = BAR2
  succ (FOO1) = BAR1
  succ (BAR1) = BAZ
  succ (BAZ) = FOO2
  succ (FOO2) = BAR2
  pred (BAR1) = FOO1
  pred (BAZ) = BAR1
  pred (FOO2) = BAZ
  pred (BAR2) = FOO2
 
instance P'.Wire TestEnumWithDupValue where
  wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
  wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.GPB TestEnumWithDupValue
 
instance P'.MessageAPI msg' (msg' -> TestEnumWithDupValue) TestEnumWithDupValue where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum TestEnumWithDupValue where
  reflectEnum = [(1, "FOO1", FOO1), (2, "BAR1", BAR1), (3, "BAZ", BAZ), (1, "FOO2", FOO2), (2, "BAR2", BAR2)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName "" "UnittestProto" "TestEnumWithDupValue") ["UnittestProto", "TestEnumWithDupValue.hs"]
        [(1, "FOO1"), (2, "BAR1"), (3, "BAZ"), (1, "FOO2"), (2, "BAR2")]