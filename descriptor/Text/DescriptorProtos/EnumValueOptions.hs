module Text.DescriptorProtos.EnumValueOptions (EnumValueOptions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data EnumValueOptions = EnumValueOptions{uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption,
                                         ext'field :: P'.ExtField, unknown'field :: P'.UnknownField}
                      deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage EnumValueOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage EnumValueOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable EnumValueOptions where
  mergeEmpty = EnumValueOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (EnumValueOptions x'1 x'2 x'3) (EnumValueOptions y'1 y'2 y'3)
    = EnumValueOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default EnumValueOptions where
  defaultValue = EnumValueOptions P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EnumValueOptions where
  wireSize ft' self'@(EnumValueOptions x'1 x'2 x'3)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 2 11 x'1 + P'.wireSizeExtField x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(EnumValueOptions x'1 x'2 x'3)
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
              P'.wirePutRep 7994 11 x'1
              P'.wirePutExtField x'2
              P'.wirePutUnknownField x'3
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith other'Field update'Self
        11 -> P'.getMessageWith other'Field update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
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
 
instance P'.MessageAPI msg' (msg' -> EnumValueOptions) EnumValueOptions where
  getVal m' f' = f' m'
 
instance P'.GPB EnumValueOptions
 
instance P'.ReflectDescriptor EnumValueOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"EnumValueOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumValueOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.EnumValueOptions\", baseName = \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True}"