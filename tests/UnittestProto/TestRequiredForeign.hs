module UnittestProto.TestRequiredForeign (TestRequiredForeign(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.TestRequired as UnittestProto (TestRequired)
 
data TestRequiredForeign = TestRequiredForeign{optional_message :: P'.Maybe UnittestProto.TestRequired,
                                               repeated_message :: P'.Seq UnittestProto.TestRequired, dummy :: P'.Maybe P'.Int32,
                                               unknown'field :: P'.UnknownField}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage TestRequiredForeign where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable TestRequiredForeign where
  mergeEmpty = TestRequiredForeign P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (TestRequiredForeign x'1 x'2 x'3 x'4) (TestRequiredForeign y'1 y'2 y'3 y'4)
    = TestRequiredForeign (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default TestRequiredForeign where
  defaultValue = TestRequiredForeign P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire TestRequiredForeign where
  wireSize ft' self'@(TestRequiredForeign x'1 x'2 x'3 x'4)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 1 5 x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(TestRequiredForeign x'1 x'2 x'3 x'4)
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
              P'.wirePutOpt 10 11 x'1
              P'.wirePutRep 18 11 x'2
              P'.wirePutOpt 24 5 x'3
              P'.wirePutUnknownField x'4
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap
                     (\ new'Field -> old'Self{optional_message = P'.mergeAppend (optional_message old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              2 -> P'.fmap (\ new'Field -> old'Self{repeated_message = P'.append (repeated_message old'Self) new'Field})
                     (P'.wireGet 11)
              3 -> P'.fmap (\ new'Field -> old'Self{dummy = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestRequiredForeign) TestRequiredForeign where
  getVal m' f' = f' m'
 
instance P'.GPB TestRequiredForeign
 
instance P'.ReflectDescriptor TestRequiredForeign where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequiredForeign\"}, descFilePath = [\"UnittestProto\",\"TestRequiredForeign.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequiredForeign\", baseName = \"optional_message\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequired\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequiredForeign\", baseName = \"repeated_message\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestRequired\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestRequiredForeign\", baseName = \"dummy\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"