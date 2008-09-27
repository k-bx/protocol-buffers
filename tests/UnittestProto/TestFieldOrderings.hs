module UnittestProto.TestFieldOrderings (TestFieldOrderings(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto as UnittestProto (my_extension_int, my_extension_string)
 
data TestFieldOrderings = TestFieldOrderings{my_string :: P'.Maybe P'.Utf8, my_int :: P'.Maybe P'.Int64,
                                             my_float :: P'.Maybe P'.Float, ext'field :: P'.ExtField,
                                             unknown'field :: P'.UnknownField}
                        deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage TestFieldOrderings where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage TestFieldOrderings where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestFieldOrderings where
  mergeEmpty = TestFieldOrderings P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (TestFieldOrderings x'1 x'2 x'3 x'4 x'5) (TestFieldOrderings y'1 y'2 y'3 y'4 y'5)
    = TestFieldOrderings (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
 
instance P'.Default TestFieldOrderings where
  defaultValue = TestFieldOrderings P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire TestFieldOrderings where
  wireSize ft' self'@(TestFieldOrderings x'1 x'2 x'3 x'4 x'5)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 3 x'2 + P'.wireSizeOpt 2 2 x'3 + P'.wireSizeExtField x'4 +
               P'.wireSizeUnknownField x'5)
  wirePut ft' self'@(TestFieldOrderings x'1 x'2 x'3 x'4 x'5)
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
              P'.wirePutOpt 8 3 x'2
              P'.wirePutOpt 90 9 x'1
              P'.wirePutOpt 813 2 x'3
              P'.wirePutExtField x'4
              P'.wirePutUnknownField x'5
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith other'Field update'Self
        11 -> P'.getMessageWith other'Field update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              11 -> P'.fmap (\ new'Field -> old'Self{my_string = P'.Just new'Field}) (P'.wireGet 9)
              1 -> P'.fmap (\ new'Field -> old'Self{my_int = P'.Just new'Field}) (P'.wireGet 3)
              101 -> P'.fmap (\ new'Field -> old'Self{my_float = P'.Just new'Field}) (P'.wireGet 2)
              5 -> P'.wireGetKey UnittestProto.my_extension_int old'Self
              50 -> P'.wireGetKey UnittestProto.my_extension_string old'Self
              _ -> P'.unknownField field'Number
        other'Field field'Number wire'Type old'Self
          = (if P'.or [2 <= field'Number && field'Number <= 10, 12 <= field'Number && field'Number <= 100] then P'.loadExtension
               else P'.loadUnknown)
              field'Number
              wire'Type
              old'Self
 
instance P'.MessageAPI msg' (msg' -> TestFieldOrderings) TestFieldOrderings where
  getVal m' f' = f' m'
 
instance P'.GPB TestFieldOrderings
 
instance P'.ReflectDescriptor TestFieldOrderings where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestFieldOrderings\"}, descFilePath = [\"UnittestProto\",\"TestFieldOrderings.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestFieldOrderings\", baseName = \"my_string\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestFieldOrderings\", baseName = \"my_int\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestFieldOrderings\", baseName = \"my_float\"}, fieldNumber = FieldId {getFieldId = 101}, wireTag = WireTag {getWireTag = 813}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 2},FieldId {getFieldId = 10}),(FieldId {getFieldId = 12},FieldId {getFieldId = 100})], knownKeys = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"my_extension_int\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"my_extension_string\"}, fieldNumber = FieldId {getFieldId = 50}, wireTag = WireTag {getWireTag = 402}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], storeUnknown = True}"