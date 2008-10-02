module Text.DescriptorProtos.FieldOptions (FieldOptions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions (CType)
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data FieldOptions = FieldOptions{ctype :: P'.Maybe DescriptorProtos.FieldOptions.CType, experimental_map_key :: P'.Maybe P'.Utf8,
                                 uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption, ext'field :: P'.ExtField}
                  deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage FieldOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.Mergeable FieldOptions where
  mergeEmpty = FieldOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (FieldOptions x'1 x'2 x'3 x'4) (FieldOptions y'1 y'2 y'3 y'4)
    = FieldOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default FieldOptions where
  defaultValue = FieldOptions P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire FieldOptions where
  wireSize ft' self'@(FieldOptions x'1 x'2 x'3 x'4)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 2 11 x'3 + P'.wireSizeExtField x'4)
  wirePut ft' self'@(FieldOptions x'1 x'2 x'3 x'4)
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
              P'.wirePutOpt 8 14 x'1
              P'.wirePutOpt 74 9 x'2
              P'.wirePutRep 7994 11 x'3
              P'.wirePutExtField x'4
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith other'Field update'Self
        11 -> P'.getMessageWith other'Field update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{ctype = P'.Just new'Field}) (P'.wireGet 14)
              9 -> P'.fmap (\ new'Field -> old'Self{experimental_map_key = P'.Just new'Field}) (P'.wireGet 9)
              999
                -> P'.fmap (\ new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
        other'Field field'Number wire'Type old'Self
          = (if P'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then P'.loadExtension else P'.unknown)
              field'Number
              wire'Type
              old'Self
 
instance P'.MessageAPI msg' (msg' -> FieldOptions) FieldOptions where
  getVal m' f' = f' m'
 
instance P'.GPB FieldOptions
 
instance P'.ReflectDescriptor FieldOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FieldOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FieldOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldOptions\", baseName = \"ctype\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldOptions\", baseName = \"CType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldOptions\", baseName = \"experimental_map_key\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FieldOptions\", baseName = \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = False}"