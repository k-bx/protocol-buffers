module UnittestProto.ForeignEnum (ForeignEnum(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ForeignEnum = FOREIGN_FOO
                 | FOREIGN_BAR
                 | FOREIGN_BAZ
                 deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ForeignEnum
 
instance P'.Bounded ForeignEnum where
  minBound = FOREIGN_FOO
  maxBound = FOREIGN_BAZ
 
instance P'.Default ForeignEnum where
  defaultValue = FOREIGN_FOO
 
instance P'.Enum ForeignEnum where
  fromEnum (FOREIGN_FOO) = 4
  fromEnum (FOREIGN_BAR) = 5
  fromEnum (FOREIGN_BAZ) = 6
  toEnum 4 = FOREIGN_FOO
  toEnum 5 = FOREIGN_BAR
  toEnum 6 = FOREIGN_BAZ
  succ (FOREIGN_FOO) = FOREIGN_BAR
  succ (FOREIGN_BAR) = FOREIGN_BAZ
  pred (FOREIGN_BAR) = FOREIGN_FOO
  pred (FOREIGN_BAZ) = FOREIGN_BAR
 
instance P'.Wire ForeignEnum where
  wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
  wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.GPB ForeignEnum
 
instance P'.MessageAPI msg' (msg' -> ForeignEnum) ForeignEnum where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ForeignEnum where
  reflectEnum = [(4, "FOREIGN_FOO", FOREIGN_FOO), (5, "FOREIGN_BAR", FOREIGN_BAR), (6, "FOREIGN_BAZ", FOREIGN_BAZ)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.ProtoName "" "UnittestProto" "ForeignEnum") ["UnittestProto", "ForeignEnum.hs"]
        [(4, "FOREIGN_FOO"), (5, "FOREIGN_BAR"), (6, "FOREIGN_BAZ")]