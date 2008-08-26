module Text.DescriptorProtos.FileOptions.OptimizeMode
       (OptimizeMode(..)) where
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
        wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
        wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
        wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.GPB OptimizeMode
 
instance P'.ReflectEnum OptimizeMode where
        reflectEnum = [(1, "SPEED", SPEED), (2, "CODE_SIZE", CODE_SIZE)]
        reflectEnumInfo _
          = P'.EnumInfo
              (P'.ProtoName "Text" "DescriptorProtos.FileOptions" "OptimizeMode")
              [(1, "SPEED"), (2, "CODE_SIZE")]