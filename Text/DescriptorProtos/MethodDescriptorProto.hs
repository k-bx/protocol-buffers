module Text.DescriptorProtos.MethodDescriptorProto
       (MethodDescriptorProto(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.MethodOptions
       as DescriptorProtos (MethodOptions)
 
data MethodDescriptorProto = MethodDescriptorProto{name ::
                                                   P'.Maybe P'.Utf8,
                                                   input_type :: P'.Maybe P'.Utf8,
                                                   output_type :: P'.Maybe P'.Utf8,
                                                   options ::
                                                   P'.Maybe DescriptorProtos.MethodOptions}
                           deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable MethodDescriptorProto where
        mergeEmpty
          = MethodDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
              P'.mergeEmpty
        mergeAppend (MethodDescriptorProto x'1 x'2 x'3 x'4)
          (MethodDescriptorProto y'1 y'2 y'3 y'4)
          = MethodDescriptorProto (P'.mergeAppend x'1 y'1)
              (P'.mergeAppend x'2 y'2)
              (P'.mergeAppend x'3 y'3)
              (P'.mergeAppend x'4 y'4)
 
instance P'.Default MethodDescriptorProto where
        defaultValue
          = MethodDescriptorProto (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
 
instance P'.Wire MethodDescriptorProto where
        wireSize 11 (MethodDescriptorProto x'1 x'2 x'3 x'4)
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 +
               P'.wireSizeOpt 1 9 x'3
               + P'.wireSizeOpt 1 11 x'4)
        wirePut 11 self'@(MethodDescriptorProto x'1 x'2 x'3 x'4)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 10 9 x'1
               P'.wirePutOpt 18 9 x'2
               P'.wirePutOpt 26 9 x'3
               P'.wirePutOpt 34 11 x'4
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field})
                               (P'.wireGet 9)
                        2 -> P'.fmap
                               (\ new'Field -> old'Self{input_type = P'.Just new'Field})
                               (P'.wireGet 9)
                        3 -> P'.fmap
                               (\ new'Field -> old'Self{output_type = P'.Just new'Field})
                               (P'.wireGet 9)
                        4 -> P'.fmap
                               (\ new'Field ->
                                  old'Self{options =
                                             P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                               (P'.wireGet 11)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor MethodDescriptorProto where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MethodDescriptorProto\"}, fields = fromList [FieldInfo {fieldName = \"name\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"input_type\", fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"output_type\", fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"options\", fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.MethodOptions\", hsRawDefault = Nothing, hsDefault = Nothing}]}"