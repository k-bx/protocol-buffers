module Text.DescriptorProtos.EnumDescriptorProto
       (EnumDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.EnumOptions
       as DescriptorProtos (EnumOptions)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto
       as DescriptorProtos (EnumValueDescriptorProto)
 
data EnumDescriptorProto = EnumDescriptorProto{name ::
                                               P'.Maybe P'.Utf8,
                                               value ::
                                               P'.Seq DescriptorProtos.EnumValueDescriptorProto,
                                               options :: P'.Maybe DescriptorProtos.EnumOptions}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable EnumDescriptorProto where
        mergeEmpty
          = EnumDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
        mergeAppend (EnumDescriptorProto x'1 x'2 x'3)
          (EnumDescriptorProto y'1 y'2 y'3)
          = EnumDescriptorProto (P'.mergeAppend x'1 y'1)
              (P'.mergeAppend x'2 y'2)
              (P'.mergeAppend x'3 y'3)
 
instance P'.Default EnumDescriptorProto where
        defaultValue
          = EnumDescriptorProto (P'.Just P'.defaultValue) (P'.defaultValue)
              (P'.Just P'.defaultValue)
 
instance P'.Wire EnumDescriptorProto where
        wireSize 11 (EnumDescriptorProto x'1 x'2 x'3)
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 +
               P'.wireSizeOpt 1 11 x'3)
        wirePut 11 self'@(EnumDescriptorProto x'1 x'2 x'3)
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
                                  old'Self{value = P'.append (value old'Self) new'Field})
                               (P'.wireGet 11)
                        3 -> P'.fmap
                               (\ new'Field ->
                                  old'Self{options =
                                             P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                               (P'.wireGet 11)
                        _ -> P'.unknownField field'Number
 
instance P'.GPB EnumDescriptorProto
 
instance P'.ReflectDescriptor EnumDescriptorProto where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumDescriptorProto\"}, fields = fromList [FieldInfo {fieldName = \"name\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"value\", fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.EnumValueDescriptorProto\", hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"options\", fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just \"DescriptorProtos.EnumOptions\", hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = []}"