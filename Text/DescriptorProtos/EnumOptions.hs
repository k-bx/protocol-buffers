module Text.DescriptorProtos.EnumOptions (EnumOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data EnumOptions = EnumOptions{}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumOptions where
        mergeEmpty = EnumOptions
        mergeAppend (EnumOptions) (EnumOptions) = EnumOptions
 
instance P'.Default EnumOptions where
        defaultValue = EnumOptions
 
instance P'.Wire EnumOptions where
        wireSize 11 (EnumOptions) = 0
        wirePut 11 self'@(EnumOptions)
          = do P'.putSize (P'.wireSize 11 self')
               P'.return ()
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        _ -> P'.unknownField field'Number
 
instance P'.GPB EnumOptions
 
instance P'.ReflectDescriptor EnumOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumOptions\"}, fields = fromList [], keys = fromList [], extRanges = []}"