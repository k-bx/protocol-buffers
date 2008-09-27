module UnittestProto.FooRequest (FooRequest(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data FooRequest = FooRequest{unknown'field :: P'.UnknownField}
                deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage FooRequest where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable FooRequest where
  mergeEmpty = FooRequest P'.mergeEmpty
  mergeAppend (FooRequest x'1) (FooRequest y'1) = FooRequest (P'.mergeAppend x'1 y'1)
 
instance P'.Default FooRequest where
  defaultValue = FooRequest P'.defaultValue
 
instance P'.Wire FooRequest where
  wireSize ft' self'@(FooRequest x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeUnknownField x'1)
  wirePut ft' self'@(FooRequest x'1)
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
 
instance P'.MessageAPI msg' (msg' -> FooRequest) FooRequest where
  getVal m' f' = f' m'
 
instance P'.GPB FooRequest
 
instance P'.ReflectDescriptor FooRequest where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"FooRequest\"}, descFilePath = [\"UnittestProto\",\"FooRequest.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"