module Text.DescriptorProtos.FileOptions.OptimizeMode (OptimizeMode(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data OptimizeMode = SPEED
                  | CODE_SIZE
                  deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable OptimizeMode
 
instance P'.Bounded OptimizeMode where
  minBound = SPEED
  maxBound = CODE_SIZE
 
instance P'.Default OptimizeMode where
  defaultValue = SPEED
 
instance P'.Enum OptimizeMode where
  fromEnum (SPEED) = 1
  fromEnum (CODE_SIZE) = 2
  toEnum 1 = SPEED
  toEnum 2 = CODE_SIZE
  succ (SPEED) = CODE_SIZE
  pred (CODE_SIZE) = SPEED
 
instance P'.Wire OptimizeMode where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB OptimizeMode
 
instance P'.MessageAPI msg' (msg' -> OptimizeMode) OptimizeMode where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum OptimizeMode where
  reflectEnum = [(1, "SPEED", SPEED), (2, "CODE_SIZE", CODE_SIZE)]
  reflectEnumInfo _
    = P'.EnumInfo (P'.makePNF (P'.pack "MakeReflections.xxx") ["Text"] ["DescriptorProtos","FileOptions"] "OptimizeMode")
        ["Text", "DescriptorProtos", "FileOptions", "OptimizeMode.hs"]
        [(1, "SPEED"), (2, "CODE_SIZE")]