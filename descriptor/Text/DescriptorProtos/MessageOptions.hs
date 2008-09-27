module Text.DescriptorProtos.MessageOptions (MessageOptions(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data MessageOptions = MessageOptions{message_set_wire_format :: P'.Maybe P'.Bool, unknown'field :: P'.UnknownField}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage MessageOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable MessageOptions where
  mergeEmpty = MessageOptions P'.mergeEmpty P'.mergeEmpty
  mergeAppend (MessageOptions x'1 x'2) (MessageOptions y'1 y'2) = MessageOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default MessageOptions where
  defaultValue = MessageOptions (P'.Just P'.False) P'.defaultValue
 
instance P'.Wire MessageOptions where
  wireSize ft' self'@(MessageOptions x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 8 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(MessageOptions x'1 x'2)
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
              P'.wirePutOpt 8 8 x'1
              P'.wirePutUnknownField x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
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
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MessageOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"MessageOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.MessageOptions\", baseName = \"message_set_wire_format\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"