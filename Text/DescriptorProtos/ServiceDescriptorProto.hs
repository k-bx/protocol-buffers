module Text.DescriptorProtos.ServiceDescriptorProto
       (ServiceDescriptorProto(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.MethodDescriptorProto
       as DescriptorProtos (MethodDescriptorProto)
import qualified Text.DescriptorProtos.ServiceOptions
       as DescriptorProtos (ServiceOptions)
 
data ServiceDescriptorProto = ServiceDescriptorProto{name ::
                                                     P'.Maybe P'.Utf8,
                                                     method ::
                                                     P'.Seq DescriptorProtos.MethodDescriptorProto,
                                                     options ::
                                                     P'.Maybe DescriptorProtos.ServiceOptions}
                            deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable ServiceDescriptorProto where
        mergeEmpty
          = ServiceDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        mergeAppend (ServiceDescriptorProto x'1 x'2 x'3)
          (ServiceDescriptorProto y'1 y'2 y'3)
          = ServiceDescriptorProto (P'.mergeAppend x'1 y'1)
              (P'.mergeAppend x'2 y'2)
              (P'.mergeAppend x'3 y'3)
 
instance P'.Default ServiceDescriptorProto where
        defaultValue
          = ServiceDescriptorProto (P'.Just P'.defaultValue)
              (P'.defaultValue)
              (P'.Just P'.defaultValue)
 
instance P'.Wire ServiceDescriptorProto where
        wireSize 11 (ServiceDescriptorProto x'1 x'2 x'3)
          = P'.lenSize
              (0 + P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 +
                 P'.wireSizeOpt 1 11 x'3)
        wirePut 11 self'@(ServiceDescriptorProto x'1 x'2 x'3)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 10 9 x'1
               P'.wirePutRep 18 11 x'2
               P'.wirePutOpt 26 11 x'3
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field})
                               (P'.wireGet 9)
                        2 -> P'.fmap
                               (\ new'Field ->
                                  old'Self{method = P'.append (method old'Self) new'Field})
                               (P'.wireGet 11)
                        3 -> P'.fmap (\ new'Field -> old'Self{options = P'.Just new'Field})
                               (P'.wireGet 11)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor ServiceDescriptorProto where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"ServiceDescriptorProto\"}, fields = fromList [FieldInfo {fieldName = \"name\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"method\", fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.MethodDescriptorProto\", hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"options\", fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.ServiceOptions\", hsRawDefault = Nothing, hsDefault = Nothing}]}"