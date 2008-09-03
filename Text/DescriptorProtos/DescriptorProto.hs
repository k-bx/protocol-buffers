module Text.DescriptorProtos.DescriptorProto (DescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.MessageOptions as DescriptorProtos (MessageOptions)
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as DescriptorProtos.DescriptorProto (ExtensionRange)
 
data DescriptorProto = DescriptorProto{name :: P'.Maybe P'.Utf8, field :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                       extension :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                       nested_type :: P'.Seq DescriptorProto,
                                       enum_type :: P'.Seq DescriptorProtos.EnumDescriptorProto,
                                       extension_range :: P'.Seq DescriptorProtos.DescriptorProto.ExtensionRange,
                                       options :: P'.Maybe DescriptorProtos.MessageOptions}
                     deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable DescriptorProto where
  mergeEmpty = DescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7) (DescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7)
    = DescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
 
instance P'.Default DescriptorProto where
  defaultValue
    = DescriptorProto (P'.Just P'.defaultValue) P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
        (P'.Just P'.defaultValue)
 
instance P'.Wire DescriptorProto where
  wireSize 11 (DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7)
    = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4 +
         P'.wireSizeRep 1 11 x'5
         + P'.wireSizeRep 1 11 x'6
         + P'.wireSizeOpt 1 11 x'7)
  wirePut 11 self'@(DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7)
    = do
        P'.putSize (P'.wireSize 11 self')
        P'.wirePutOpt 10 9 x'1
        P'.wirePutRep 18 11 x'2
        P'.wirePutRep 50 11 x'3
        P'.wirePutRep 26 11 x'4
        P'.wirePutRep 34 11 x'5
        P'.wirePutRep 42 11 x'6
        P'.wirePutOpt 58 11 x'7
  wireGet 11 = P'.getMessage update'Self
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
              2 -> P'.fmap (\ new'Field -> old'Self{field = P'.append (field old'Self) new'Field}) (P'.wireGet 11)
              6 -> P'.fmap (\ new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
              3 -> P'.fmap (\ new'Field -> old'Self{nested_type = P'.append (nested_type old'Self) new'Field}) (P'.wireGet 11)
              4 -> P'.fmap (\ new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
              5 -> P'.fmap (\ new'Field -> old'Self{extension_range = P'.append (extension_range old'Self) new'Field})
                     (P'.wireGet 11)
              7 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> DescriptorProto) DescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB DescriptorProto
 
instance P'.ReflectDescriptor DescriptorProto where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"DescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"DescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"field\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"extension\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"nested_type\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"enum_type\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"extension_range\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"ExtensionRange\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"options\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MessageOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"