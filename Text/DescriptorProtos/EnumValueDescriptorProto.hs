module Text.DescriptorProtos.EnumValueDescriptorProto
       (EnumValueDescriptorProto(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.EnumValueOptions
       as DescriptorProtos (EnumValueOptions)
 
data EnumValueDescriptorProto = EnumValueDescriptorProto{name ::
                                                         P'.Maybe P'.Utf8,
                                                         number :: P'.Maybe P'.Int32,
                                                         options ::
                                                         P'.Maybe DescriptorProtos.EnumValueOptions}
                              deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable EnumValueDescriptorProto where
        mergeEmpty
          = EnumValueDescriptorProto P'.mergeEmpty P'.mergeEmpty
              P'.mergeEmpty
        mergeAppend (EnumValueDescriptorProto x'1 x'2 x'3)
          (EnumValueDescriptorProto y'1 y'2 y'3)
          = EnumValueDescriptorProto (P'.mergeAppend x'1 y'1)
              (P'.mergeAppend x'2 y'2)
              (P'.mergeAppend x'3 y'3)
 
instance P'.Default EnumValueDescriptorProto where
        defaultValue
          = EnumValueDescriptorProto (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
 
instance P'.Wire EnumValueDescriptorProto where
        wireSize 11 (EnumValueDescriptorProto x'1 x'2 x'3)
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 5 x'2 +
               P'.wireSizeOpt 1 11 x'3)
        wirePut 11 self'@(EnumValueDescriptorProto x'1 x'2 x'3)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 10 9 x'1
               P'.wirePutOpt 16 5 x'2
               P'.wirePutOpt 26 11 x'3
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field})
                               (P'.wireGet 9)
                        2 -> P'.fmap (\ new'Field -> old'Self{number = P'.Just new'Field})
                               (P'.wireGet 5)
                        3 -> P'.fmap (\ new'Field -> old'Self{options = P'.Just new'Field})
                               (P'.wireGet 11)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor EnumValueDescriptorProto where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueDescriptorProto\"}, fields = fromList [FieldInfo {fieldName = \"name\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"number\", fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"options\", fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.EnumValueOptions\", hsRawDefault = Nothing, hsDefault = Nothing}]}"