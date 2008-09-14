module Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProtos (DescriptorProto)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.FileOptions as DescriptorProtos (FileOptions)
import qualified Text.DescriptorProtos.ServiceDescriptorProto as DescriptorProtos (ServiceDescriptorProto)
 
data FileDescriptorProto = FileDescriptorProto{name :: P'.Maybe P'.Utf8, package :: P'.Maybe P'.Utf8, dependency :: P'.Seq P'.Utf8,
                                               message_type :: P'.Seq DescriptorProtos.DescriptorProto,
                                               enum_type :: P'.Seq DescriptorProtos.EnumDescriptorProto,
                                               service :: P'.Seq DescriptorProtos.ServiceDescriptorProto,
                                               extension :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                               options :: P'.Maybe DescriptorProtos.FileOptions}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable FileDescriptorProto where
  mergeEmpty
    = FileDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        P'.mergeEmpty
  mergeAppend (FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (FileDescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
    = FileDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
        (P'.mergeAppend x'8 y'8)
 
instance P'.Default FileDescriptorProto where
  defaultValue
    = FileDescriptorProto (P'.Just P'.defaultValue) (P'.Just P'.defaultValue) P'.defaultValue P'.defaultValue P'.defaultValue
        P'.defaultValue
        P'.defaultValue
        (P'.Just P'.defaultValue)
 
instance P'.Wire FileDescriptorProto where
  wireSize ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
    = case ft' of
        10 -> calc'Size
        11 -> calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 9 x'3 + P'.wireSizeRep 1 11 x'4 +
               P'.wireSizeRep 1 11 x'5
               + P'.wireSizeRep 1 11 x'6
               + P'.wireSizeRep 1 11 x'7
               + P'.wireSizeOpt 1 11 x'8)
  wirePut ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 11 self')
               put'Fields
        _ -> P'.wirePutErr ft' self'
    where
        put'Fields
          = do
              P'.wirePutOpt 10 9 x'1
              P'.wirePutOpt 18 9 x'2
              P'.wirePutRep 26 9 x'3
              P'.wirePutRep 34 11 x'4
              P'.wirePutRep 42 11 x'5
              P'.wirePutRep 50 11 x'6
              P'.wirePutRep 58 11 x'7
              P'.wirePutOpt 66 11 x'8
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
              2 -> P'.fmap (\ new'Field -> old'Self{package = P'.Just new'Field}) (P'.wireGet 9)
              3 -> P'.fmap (\ new'Field -> old'Self{dependency = P'.append (dependency old'Self) new'Field}) (P'.wireGet 9)
              4 -> P'.fmap (\ new'Field -> old'Self{message_type = P'.append (message_type old'Self) new'Field}) (P'.wireGet 11)
              5 -> P'.fmap (\ new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
              6 -> P'.fmap (\ new'Field -> old'Self{service = P'.append (service old'Self) new'Field}) (P'.wireGet 11)
              7 -> P'.fmap (\ new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
              8 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> FileDescriptorProto) FileDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB FileDescriptorProto
 
instance P'.ReflectDescriptor FileDescriptorProto where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"package\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"dependency\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"message_type\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"enum_type\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"service\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"ServiceDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"extension\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorProto\", baseName = \"options\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"