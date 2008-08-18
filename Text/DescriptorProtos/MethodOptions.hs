module Text.DescriptorProtos.MethodOptions (MethodOptions(..))
       where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data MethodOptions = MethodOptions{}
                   deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable MethodOptions where
        mergeEmpty = MethodOptions
        mergeAppend (MethodOptions) (MethodOptions) = MethodOptions
 
instance P'.Default MethodOptions where
        defaultValue = MethodOptions
 
instance P'.Wire MethodOptions where
        wireSize 11 (MethodOptions) = 0
        wirePut 11 self'@(MethodOptions)
          = do P'.putSize (P'.wireSize 11 self')
               P'.return ()
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor MethodOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MethodOptions\"}, fields = fromList []}"