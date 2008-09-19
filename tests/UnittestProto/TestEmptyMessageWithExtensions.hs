module UnittestProto.TestEmptyMessageWithExtensions (TestEmptyMessageWithExtensions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestEmptyMessageWithExtensions = TestEmptyMessageWithExtensions{ext'field :: P'.ExtField}
                                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage TestEmptyMessageWithExtensions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable TestEmptyMessageWithExtensions where
  mergeEmpty = TestEmptyMessageWithExtensions P'.mergeEmpty
  mergeAppend (TestEmptyMessageWithExtensions x'1) (TestEmptyMessageWithExtensions y'1)
    = TestEmptyMessageWithExtensions (P'.mergeAppend x'1 y'1)
 
instance P'.Default TestEmptyMessageWithExtensions where
  defaultValue = TestEmptyMessageWithExtensions P'.defaultValue
 
instance P'.Wire TestEmptyMessageWithExtensions where
  wireSize ft' self'@(TestEmptyMessageWithExtensions x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeExtField x'1)
  wirePut ft' self'@(TestEmptyMessageWithExtensions x'1)
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
              P'.wirePutExtField x'1
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageExt update'Self
        11 -> P'.getMessageExt update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestEmptyMessageWithExtensions) TestEmptyMessageWithExtensions where
  getVal m' f' = f' m'
 
instance P'.GPB TestEmptyMessageWithExtensions
 
instance P'.ReflectDescriptor TestEmptyMessageWithExtensions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestEmptyMessageWithExtensions\"}, descFilePath = [\"UnittestProto\",\"TestEmptyMessageWithExtensions.hs\"], isGroup = False, fields = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList []}"