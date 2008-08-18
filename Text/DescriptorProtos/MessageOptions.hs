module Text.DescriptorProtos.MessageOptions (MessageOptions(..))
       where
import Prelude ((+), (++))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data MessageOptions = MessageOptions{message_set_wire_format ::
                                     P'.Maybe P'.Bool}
                    deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable MessageOptions where
        mergeEmpty = MessageOptions P'.mergeEmpty
        mergeAppend (MessageOptions x'1) (MessageOptions y'1)
          = MessageOptions (P'.mergeAppend x'1 y'1)
 
instance P'.Default MessageOptions where
        defaultValue = MessageOptions (P'.Just (P'.False))
 
instance P'.Wire MessageOptions where
        wireSize 11 (MessageOptions x'1) = (P'.wireSizeOpt 1 8 x'1)
        wirePut 11 self'@(MessageOptions x'1)
          = do P'.putSize (P'.wireSize 11 self')
               P'.wirePutOpt 8 8 x'1
        wireGet 11 = P'.getMessage update'Self
          where update'Self field'Number old'Self
                  = case field'Number of
                        1 -> P'.fmap
                               (\ new'Field ->
                                  old'Self{message_set_wire_format = P'.Just new'Field})
                               (P'.wireGet 8)
                        _ -> P'.unknownField field'Number
 
instance P'.ReflectDescriptor MessageOptions where
        reflectDescriptorInfo _
          = P'.read
              "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MessageOptions\"}, fields = fromList [FieldInfo {fieldName = \"message_set_wire_format\", fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)}]}"