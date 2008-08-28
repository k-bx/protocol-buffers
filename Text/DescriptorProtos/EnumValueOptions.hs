module Text.DescriptorProtos.EnumValueOptions
       (EnumValueOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data EnumValueOptions = EnumValueOptions{}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumValueOptions where
        mergeEmpty = EnumValueOptions
        mergeAppend (EnumValueOptions) (EnumValueOptions)
          = EnumValueOptions
 
instance P'.Default EnumValueOptions where
        defaultValue = EnumValueOptions
 
instance P'.Wire EnumValueOptions where
        wireSize 11 (EnumValueOptions) = 0
        wirePut 11 self'@(EnumValueOptions)
          = do P'.putSize (P'.wireSize 11 self')
               P'.return ()
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        _ -> P'.unknownField field'Number
 
instance P'.GPB EnumValueOptions
 
instance P'.ReflectDescriptor EnumValueOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueOptions\"}, isGroup = False, fields = fromList [], keys = fromList [], extRanges = []}"