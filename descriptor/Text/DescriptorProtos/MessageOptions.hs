module Text.DescriptorProtos.MessageOptions (MessageOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data MessageOptions = MessageOptions{message_set_wire_format :: P'.Maybe P'.Bool}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable MessageOptions where
  mergeEmpty = MessageOptions P'.mergeEmpty
  mergeAppend (MessageOptions x'1) (MessageOptions y'1) = MessageOptions (P'.mergeAppend x'1 y'1)
 
instance P'.Default MessageOptions where
  defaultValue = MessageOptions (P'.Just P'.False)
 
instance P'.Wire MessageOptions where
  wireSize ft' self'@(MessageOptions x'1)
    = case ft' of
        10 -> calc'Size
        11 -> calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 8 x'1)
  wirePut ft' self'@(MessageOptions x'1)
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
              P'.wirePutOpt 8 8 x'1
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{message_set_wire_format = P'.Just new'Field}) (P'.wireGet 8)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> MessageOptions) MessageOptions where
  getVal m' f' = f' m'
 
instance P'.GPB MessageOptions
 
instance P'.ReflectDescriptor MessageOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MessageOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"MessageOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.MessageOptions\", baseName = \"message_set_wire_format\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)}], keys = fromList [], extRanges = [], knownKeys = fromList []}"