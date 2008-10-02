module Text.DescriptorProtos.MethodOptions (MethodOptions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data MethodOptions = MethodOptions{uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption, ext'field :: P'.ExtField}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage MethodOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable MethodOptions where
  mergeEmpty = MethodOptions P'.mergeEmpty P'.mergeEmpty
  mergeAppend (MethodOptions x'1 x'2) (MethodOptions y'1 y'2) = MethodOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default MethodOptions where
  defaultValue = MethodOptions P'.defaultValue P'.defaultValue
 
instance P'.Wire MethodOptions where
  wireSize ft' self'@(MethodOptions x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 2 11 x'1 + P'.wireSizeExtField x'2)
  wirePut ft' self'@(MethodOptions x'1 x'2)
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
          = (if P'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then P'.loadExtension else P'.unknown)
              field'Number
              wire'Type
              old'Self
 
instance P'.MessageAPI msg' (msg' -> MethodOptions) MethodOptions where
  getVal m' f' = f' m'
 
instance P'.GPB MethodOptions
 
instance P'.ReflectDescriptor MethodOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"MethodOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"MethodOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.MethodOptions\", baseName = \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False}"