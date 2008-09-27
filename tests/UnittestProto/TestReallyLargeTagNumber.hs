module UnittestProto.TestReallyLargeTagNumber (TestReallyLargeTagNumber(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data TestReallyLargeTagNumber = TestReallyLargeTagNumber{a :: P'.Maybe P'.Int32, bb :: P'.Maybe P'.Int32,
                                                         unknown'field :: P'.UnknownField}
                              deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestReallyLargeTagNumber where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestReallyLargeTagNumber where
  mergeEmpty = TestReallyLargeTagNumber P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (TestReallyLargeTagNumber x'1 x'2 x'3) (TestReallyLargeTagNumber y'1 y'2 y'3)
    = TestReallyLargeTagNumber (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default TestReallyLargeTagNumber where
  defaultValue = TestReallyLargeTagNumber P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire TestReallyLargeTagNumber where
  wireSize ft' self'@(TestReallyLargeTagNumber x'1 x'2 x'3)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 5 5 x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(TestReallyLargeTagNumber x'1 x'2 x'3)
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
              P'.wirePutOpt 8 5 x'1
              P'.wirePutOpt 2147483640 5 x'2
              P'.wirePutUnknownField x'3
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{a = P'.Just new'Field}) (P'.wireGet 5)
              268435455 -> P'.fmap (\ new'Field -> old'Self{bb = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestReallyLargeTagNumber) TestReallyLargeTagNumber where
  getVal m' f' = f' m'
 
instance P'.GPB TestReallyLargeTagNumber
 
instance P'.ReflectDescriptor TestReallyLargeTagNumber where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestReallyLargeTagNumber\"}, descFilePath = [\"UnittestProto\",\"TestReallyLargeTagNumber.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestReallyLargeTagNumber\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestReallyLargeTagNumber\", baseName = \"bb\"}, fieldNumber = FieldId {getFieldId = 268435455}, wireTag = WireTag {getWireTag = 2147483640}, wireTagLength = 5, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"