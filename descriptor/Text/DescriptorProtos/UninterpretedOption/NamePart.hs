module Text.DescriptorProtos.UninterpretedOption.NamePart (NamePart(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data NamePart = NamePart{name_part :: P'.Utf8, is_extension :: P'.Bool}
              deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable NamePart where
  mergeEmpty = NamePart P'.mergeEmpty P'.mergeEmpty
  mergeAppend (NamePart x'1 x'2) (NamePart y'1 y'2) = NamePart (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default NamePart where
  defaultValue = NamePart P'.defaultValue P'.defaultValue
 
instance P'.Wire NamePart where
  wireSize ft' self'@(NamePart x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 8 x'2)
  wirePut ft' self'@(NamePart x'1 x'2)
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
              P'.wirePutReq 10 9 x'1
              P'.wirePutReq 16 8 x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.unknown update'Self
        11 -> P'.getMessageWith P'.unknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{name_part = new'Field}) (P'.wireGet 9)
              2 -> P'.fmap (\ new'Field -> old'Self{is_extension = new'Field}) (P'.wireGet 8)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> NamePart) NamePart where
  getVal m' f' = f' m'
 
instance P'.GPB NamePart
 
instance P'.ReflectDescriptor NamePart where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption\", baseName = \"NamePart\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"UninterpretedOption\",\"NamePart.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption.NamePart\", baseName = \"name_part\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = True, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.UninterpretedOption.NamePart\", baseName = \"is_extension\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, wireTagLength = 1, isRequired = True, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"