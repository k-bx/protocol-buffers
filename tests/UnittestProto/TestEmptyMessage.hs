module UnittestProto.TestEmptyMessage (TestEmptyMessage(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestEmptyMessage = TestEmptyMessage{unknown'field :: P'.UnknownField}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestEmptyMessage where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestEmptyMessage where
  mergeEmpty = TestEmptyMessage P'.mergeEmpty
  mergeAppend (TestEmptyMessage x'1) (TestEmptyMessage y'1) = TestEmptyMessage (P'.mergeAppend x'1 y'1)
 
instance P'.Default TestEmptyMessage where
  defaultValue = TestEmptyMessage P'.defaultValue
 
instance P'.Wire TestEmptyMessage where
  wireSize ft' self'@(TestEmptyMessage x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeUnknownField x'1)
  wirePut ft' self'@(TestEmptyMessage x'1)
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
 
instance P'.MessageAPI msg' (msg' -> TestEmptyMessage) TestEmptyMessage where
  getVal m' f' = f' m'
 
instance P'.GPB TestEmptyMessage
 
instance P'.ReflectDescriptor TestEmptyMessage where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestEmptyMessage\"}, descFilePath = [\"UnittestProto\",\"TestEmptyMessage.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"