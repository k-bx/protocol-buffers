module Text.DescriptorProtos.DescriptorProto.ExtensionRange
       (ExtensionRange(..)) where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ExtensionRange = ExtensionRange{start :: P'.Maybe P'.Int32,
                                     end :: P'.Maybe P'.Int32}
                    deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable ExtensionRange where
        mergeEmpty = ExtensionRange P'.mergeEmpty P'.mergeEmpty
        mergeAppend (ExtensionRange x'1 x'2) (ExtensionRange y'1 y'2)
          = ExtensionRange (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ExtensionRange where
        defaultValue
          = ExtensionRange (P'.Just P'.defaultValue)
              (P'.Just P'.defaultValue)
 
instance P'.Wire ExtensionRange where
        wireSize 11 (ExtensionRange x'1 x'2)
          = P'.lenSize (0 + P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 5 x'2)
        wirePut 11 self'@(ExtensionRange x'1 x'2)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 8 5 x'1
               P'.wirePutOpt 16 5 x'2
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap (\ new'Field -> old'Self{start = P'.Just new'Field})
                               (P'.wireGet 5)
                        2 -> P'.fmap (\ new'Field -> old'Self{end = P'.Just new'Field})
                               (P'.wireGet 5)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor ExtensionRange where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.DescriptorProto\", baseName = \"ExtensionRange\"}, fields = fromList [FieldInfo {fieldName = \"start\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = \"end\", fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}]}"