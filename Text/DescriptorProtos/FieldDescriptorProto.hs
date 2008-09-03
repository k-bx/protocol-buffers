module Text.DescriptorProtos.FieldDescriptorProto (FieldDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FieldOptions as DescriptorProtos (FieldOptions)
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as DescriptorProtos.FieldDescriptorProto (Label)
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as DescriptorProtos.FieldDescriptorProto (Type)
 
data FieldDescriptorProto = FieldDescriptorProto{name :: P'.Maybe P'.Utf8, number :: P'.Maybe P'.Int32,
                                                 label :: P'.Maybe DescriptorProtos.FieldDescriptorProto.Label,
                                                 type' :: P'.Maybe DescriptorProtos.FieldDescriptorProto.Type,
                                                 type_name :: P'.Maybe P'.Utf8, extendee :: P'.Maybe P'.Utf8,
                                                 default_value :: P'.Maybe P'.Utf8,
                                                 options :: P'.Maybe DescriptorProtos.FieldOptions}
                          deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable FieldDescriptorProto where
  mergeEmpty
    = FieldDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        P'.mergeEmpty
  mergeAppend (FieldDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (FieldDescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
    = FieldDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
        (P'.mergeAppend x'8 y'8)
 
instance P'.Default FieldDescriptorProto where
  defaultValue
    = FieldDescriptorProto (P'.Just P'.defaultValue) (P'.Just P'.defaultValue) (P'.Just P'.defaultValue) (P'.Just P'.defaultValue)
        (P'.Just P'.defaultValue)
        (P'.Just P'.defaultValue)
        (P'.Just P'.defaultValue)
        (P'.Just P'.defaultValue)
 
instance P'.Wire FieldDescriptorProto where
  wireSize 11 (FieldDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
    = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizeOpt 1 14 x'4 + P'.wireSizeOpt 1 9 x'5
         + P'.wireSizeOpt 1 9 x'6
         + P'.wireSizeOpt 1 9 x'7
         + P'.wireSizeOpt 1 11 x'8)
  wirePut 11 self'@(FieldDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
    = do
        P'.putSize (P'.wireSize 11 self')
        P'.wirePutOpt 10 9 x'1
        P'.wirePutOpt 24 5 x'2
        P'.wirePutOpt 32 14 x'3
        P'.wirePutOpt 40 14 x'4
        P'.wirePutOpt 50 9 x'5
        P'.wirePutOpt 18 9 x'6
        P'.wirePutOpt 58 9 x'7
        P'.wirePutOpt 66 11 x'8
  wireGet 11 = P'.getMessage update'Self
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
              3 -> P'.fmap (\ new'Field -> old'Self{number = P'.Just new'Field}) (P'.wireGet 5)
              4 -> P'.fmap (\ new'Field -> old'Self{label = P'.Just new'Field}) (P'.wireGet 14)
              5 -> P'.fmap (\ new'Field -> old'Self{type' = P'.Just new'Field}) (P'.wireGet 14)
              6 -> P'.fmap (\ new'Field -> old'Self{type_name = P'.Just new'Field}) (P'.wireGet 9)
              2 -> P'.fmap (\ new'Field -> old'Self{extendee = P'.Just new'Field}) (P'.wireGet 9)
              7 -> P'.fmap (\ new'Field -> old'Self{default_value = P'.Just new'Field}) (P'.wireGet 9)
              8 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> FieldDescriptorProto) FieldDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB FieldDescriptorProto
 
instance P'.ReflectDescriptor FieldDescriptorProto where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FieldDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"number\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"label\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"Label\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"type'\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"Type\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"type_name\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"extendee\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"default_value\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldDescriptorProto\", baseName = \"options\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"