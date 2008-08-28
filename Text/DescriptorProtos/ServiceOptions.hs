module Text.DescriptorProtos.ServiceOptions (ServiceOptions(..))
       where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ServiceOptions = ServiceOptions{}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ServiceOptions where
        mergeEmpty = ServiceOptions
        mergeAppend (ServiceOptions) (ServiceOptions) = ServiceOptions
 
instance P'.Default ServiceOptions where
        defaultValue = ServiceOptions
 
instance P'.Wire ServiceOptions where
        wireSize 11 (ServiceOptions) = 0
        wirePut 11 self'@(ServiceOptions)
          = do P'.putSize (P'.wireSize 11 self')
               P'.return ()
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        _ -> P'.unknownField field'Number
 
instance P'.GPB ServiceOptions
 
instance P'.ReflectDescriptor ServiceOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"ServiceOptions\"}, isGroup = False, fields = fromList [], keys = fromList [], extRanges = []}"