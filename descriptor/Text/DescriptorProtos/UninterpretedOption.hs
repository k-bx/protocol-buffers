module Text.DescriptorProtos.UninterpretedOption (UninterpretedOption(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart as DescriptorProtos.UninterpretedOption (NamePart)
 
data UninterpretedOption = UninterpretedOption{name :: P'.Seq DescriptorProtos.UninterpretedOption.NamePart,
                                               identifier_value :: P'.Maybe P'.Utf8, positive_int_value :: P'.Maybe P'.Word64,
                                               negative_int_value :: P'.Maybe P'.Int64, double_value :: P'.Maybe P'.Double,
                                               string_value :: P'.Maybe P'.ByteString}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable UninterpretedOption where
  mergeEmpty = UninterpretedOption P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6) (UninterpretedOption y'1 y'2 y'3 y'4 y'5 y'6)
    = UninterpretedOption (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
 
instance P'.Default UninterpretedOption where
  defaultValue = UninterpretedOption P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire UninterpretedOption where
  wireSize ft' self'@(UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 4 x'3 + P'.wireSizeOpt 1 3 x'4 +
               P'.wireSizeOpt 1 1 x'5
               + P'.wireSizeOpt 1 12 x'6)
  wirePut ft' self'@(UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6)
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
              P'.wirePutRep 18 11 x'1
              P'.wirePutOpt 26 9 x'2
              P'.wirePutOpt 32 4 x'3
              P'.wirePutOpt 40 3 x'4
              P'.wirePutOpt 49 1 x'5
              P'.wirePutOpt 58 12 x'6
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.unknown update'Self
        11 -> P'.getMessageWith P'.unknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              2 -> P'.fmap (\ new'Field -> old'Self{name = P'.append (name old'Self) new'Field}) (P'.wireGet 11)
              3 -> P'.fmap (\ new'Field -> old'Self{identifier_value = P'.Just new'Field}) (P'.wireGet 9)
              4 -> P'.fmap (\ new'Field -> old'Self{positive_int_value = P'.Just new'Field}) (P'.wireGet 4)
              5 -> P'.fmap (\ new'Field -> old'Self{negative_int_value = P'.Just new'Field}) (P'.wireGet 3)
              6 -> P'.fmap (\ new'Field -> old'Self{double_value = P'.Just new'Field}) (P'.wireGet 1)
              7 -> P'.fmap (\ new'Field -> old'Self{string_value = P'.Just new'Field}) (P'.wireGet 12)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> UninterpretedOption) UninterpretedOption where
  getVal m' f' = f' m'
 
instance P'.GPB UninterpretedOption
 
instance P'.ReflectDescriptor UninterpretedOption where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"UninterpretedOption.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"name\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"NamePart\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"identifier_value\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"positive_int_value\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"negative_int_value\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"double_value\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"string_value\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"