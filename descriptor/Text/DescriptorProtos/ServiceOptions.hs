module Text.DescriptorProtos.ServiceOptions (ServiceOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ServiceOptions = ServiceOptions{unknown'field :: P'.UnknownField}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage ServiceOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable ServiceOptions where
  mergeEmpty = ServiceOptions P'.mergeEmpty
  mergeAppend (ServiceOptions x'1) (ServiceOptions y'1) = ServiceOptions (P'.mergeAppend x'1 y'1)
 
instance P'.Default ServiceOptions where
  defaultValue = ServiceOptions P'.defaultValue
 
instance P'.Wire ServiceOptions where
  wireSize ft' self'@(ServiceOptions x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeUnknownField x'1)
  wirePut ft' self'@(ServiceOptions x'1)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
        _ -> P'.wirePutErr ft' self'
    where
        put'Fields
          = do
              P'.return ()
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> ServiceOptions) ServiceOptions where
  getVal m' f' = f' m'
 
instance P'.GPB ServiceOptions
 
instance P'.ReflectDescriptor ServiceOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"ServiceOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"ServiceOptions.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"