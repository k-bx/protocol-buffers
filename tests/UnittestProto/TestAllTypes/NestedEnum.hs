module UnittestProto.TestAllTypes.NestedEnum (NestedEnum(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data NestedEnum = FOO
                | BAR
                | BAZ
                deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable NestedEnum
 
instance P'.Bounded NestedEnum where
  minBound = FOO
  maxBound = BAZ
 
instance P'.Default NestedEnum where
  defaultValue = FOO
 
instance P'.Enum NestedEnum where
  fromEnum (FOO) = 1
  fromEnum (BAR) = 2
  fromEnum (BAZ) = 3
  toEnum 1 = FOO
  toEnum 2 = BAR
  toEnum 3 = BAZ
  succ (FOO) = BAR
  succ (BAR) = BAZ
  pred (BAR) = FOO
  pred (BAZ) = BAR
 
instance P'.Wire NestedEnum where
  wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
  wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.GPB NestedEnum
 
instance P'.MessageAPI msg' (msg' -> NestedEnum) NestedEnum where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum NestedEnum where
  reflectEnum = [(1, "FOO", FOO), (2, "BAR", BAR), (3, "BAZ", BAZ)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName "" "UnittestProto.TestAllTypes" "NestedEnum") ["UnittestProto", "TestAllTypes", "NestedEnum.hs"]
        [(1, "FOO"), (2, "BAR"), (3, "BAZ")]