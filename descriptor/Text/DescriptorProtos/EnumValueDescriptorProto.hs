module Text.DescriptorProtos.EnumValueDescriptorProto (EnumValueDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.EnumValueOptions as DescriptorProtos (EnumValueOptions)
 
data EnumValueDescriptorProto = EnumValueDescriptorProto{name :: P'.Maybe P'.Utf8, number :: P'.Maybe P'.Int32,
                                                         options :: P'.Maybe DescriptorProtos.EnumValueOptions}
                              deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumValueDescriptorProto where
  mergeEmpty = EnumValueDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (EnumValueDescriptorProto x'1 x'2 x'3) (EnumValueDescriptorProto y'1 y'2 y'3)
    = EnumValueDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default EnumValueDescriptorProto where
  defaultValue = EnumValueDescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EnumValueDescriptorProto where
  wireSize ft' self'@(EnumValueDescriptorProto x'1 x'2 x'3)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(EnumValueDescriptorProto x'1 x'2 x'3)
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
              P'.wirePutOpt 10 9 x'1
              P'.wirePutOpt 16 5 x'2
              P'.wirePutOpt 26 11 x'3
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.unknown update'Self
        11 -> P'.getMessageWith P'.unknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
              2 -> P'.fmap (\ new'Field -> old'Self{number = P'.Just new'Field}) (P'.wireGet 5)
              3 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> EnumValueDescriptorProto) EnumValueDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB EnumValueDescriptorProto
 
instance P'.ReflectDescriptor EnumValueDescriptorProto where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumValueDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.EnumValueDescriptorProto\", baseName = \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.EnumValueDescriptorProto\", baseName = \"number\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.EnumValueDescriptorProto\", baseName = \"options\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"