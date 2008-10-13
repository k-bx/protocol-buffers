module Text.DescriptorProtos.MessageOptions (MessageOptions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data MessageOptions = MessageOptions{message_set_wire_format :: P'.Maybe P'.Bool,
                                     uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption, ext'field :: P'.ExtField,
                                     unknown'field :: P'.UnknownField}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage MessageOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage MessageOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable MessageOptions where
  mergeEmpty = MessageOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (MessageOptions x'1 x'2 x'3 x'4) (MessageOptions y'1 y'2 y'3 y'4)
    = MessageOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default MessageOptions where
  defaultValue = MessageOptions (P'.Just P'.False) P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire MessageOptions where
  wireSize ft' self'@(MessageOptions x'1 x'2 x'3 x'4)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 8 x'1 + P'.wireSizeRep 2 11 x'2 + P'.wireSizeExtField x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(MessageOptions x'1 x'2 x'3 x'4)
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
              P'.wirePutRep 7994 11 x'2
              P'.wirePutExtField x'3
              P'.wirePutUnknownField x'4
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith other'Field update'Self
        11 -> P'.getMessageWith other'Field update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{message_set_wire_format = P'.Just new'Field}) (P'.wireGet 8)
              999
                -> P'.fmap (\ new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
        other'Field field'Number wire'Type old'Self
          = (if P'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then P'.loadExtension else
               P'.loadUnknown)
              field'Number
              wire'Type
              old'Self
 
instance P'.MessageAPI msg' (msg' -> MessageOptions) MessageOptions where
  getVal m' f' = f' m'
 
instance P'.GPB MessageOptions
 
instance P'.ReflectDescriptor MessageOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MessageOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"MessageOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.MessageOptions\", baseName = \"message_set_wire_format\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.MessageOptions\", baseName = \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True}"