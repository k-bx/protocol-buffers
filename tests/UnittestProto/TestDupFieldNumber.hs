module UnittestProto.TestDupFieldNumber (TestDupFieldNumber(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.TestDupFieldNumber.Bar as UnittestProto.TestDupFieldNumber (Bar)
import qualified UnittestProto.TestDupFieldNumber.Foo as UnittestProto.TestDupFieldNumber (Foo)
 
data TestDupFieldNumber = TestDupFieldNumber{a :: P'.Maybe P'.Int32, foo :: P'.Maybe UnittestProto.TestDupFieldNumber.Foo,
                                             bar :: P'.Maybe UnittestProto.TestDupFieldNumber.Bar, unknown'field :: P'.UnknownField}
                        deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestDupFieldNumber where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestDupFieldNumber where
  mergeEmpty = TestDupFieldNumber P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (TestDupFieldNumber x'1 x'2 x'3 x'4) (TestDupFieldNumber y'1 y'2 y'3 y'4)
    = TestDupFieldNumber (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default TestDupFieldNumber where
  defaultValue = TestDupFieldNumber P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire TestDupFieldNumber where
  wireSize ft' self'@(TestDupFieldNumber x'1 x'2 x'3 x'4)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 10 x'2 + P'.wireSizeOpt 1 10 x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(TestDupFieldNumber x'1 x'2 x'3 x'4)
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
              P'.wirePutOpt 19 10 x'2
              P'.wirePutOpt 27 10 x'3
              P'.wirePutUnknownField x'4
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{a = P'.Just new'Field}) (P'.wireGet 5)
              2 -> P'.fmap (\ new'Field -> old'Self{foo = P'.mergeAppend (foo old'Self) (P'.Just new'Field)}) (P'.wireGet 10)
              3 -> P'.fmap (\ new'Field -> old'Self{bar = P'.mergeAppend (bar old'Self) (P'.Just new'Field)}) (P'.wireGet 10)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestDupFieldNumber) TestDupFieldNumber where
  getVal m' f' = f' m'
 
instance P'.GPB TestDupFieldNumber
 
instance P'.ReflectDescriptor TestDupFieldNumber where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestDupFieldNumber\"}, descFilePath = [\"UnittestProto\",\"TestDupFieldNumber.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestDupFieldNumber\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestDupFieldNumber\", baseName = \"foo\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 19}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 10}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestDupFieldNumber\", baseName = \"Foo\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestDupFieldNumber\", baseName = \"bar\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 27}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 10}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestDupFieldNumber\", baseName = \"Bar\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"