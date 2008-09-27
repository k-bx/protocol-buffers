module UnittestProto.BarRequest (BarRequest(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data BarRequest = BarRequest{unknown'field :: P'.UnknownField}
                deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage BarRequest where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable BarRequest where
  mergeEmpty = BarRequest P'.mergeEmpty
  mergeAppend (BarRequest x'1) (BarRequest y'1) = BarRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default BarRequest where
  defaultValue = BarRequest P'.defaultValue
 
instance P'.Wire BarRequest where
  wireSize ft' self'@(BarRequest x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeUnknownField x'1)
  wirePut ft' self'@(BarRequest x'1)
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
              P'.wirePutUnknownField x'1
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> BarRequest) BarRequest where
  getVal m' f' = f' m'
 
instance P'.GPB BarRequest
 
instance P'.ReflectDescriptor BarRequest where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"BarRequest\"}, descFilePath = [\"UnittestProto\",\"BarRequest.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"