module Text.DescriptorProtos.FileOptions (FileOptions(..)) where
import Prelude ((+), (<=), (&&), ( || ))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions (OptimizeMode)
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data FileOptions = FileOptions{java_package :: P'.Maybe P'.Utf8, java_outer_classname :: P'.Maybe P'.Utf8,
                               java_multiple_files :: P'.Maybe P'.Bool,
                               optimize_for :: P'.Maybe DescriptorProtos.FileOptions.OptimizeMode,
                               uninterpreted_option :: P'.Seq DescriptorProtos.UninterpretedOption, ext'field :: P'.ExtField,
                               unknown'field :: P'.UnknownField}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.ExtendMessage FileOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage FileOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable FileOptions where
  mergeEmpty = FileOptions P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7) (FileOptions y'1 y'2 y'3 y'4 y'5 y'6 y'7)
    = FileOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
        (P'.mergeAppend x'5 y'5)
        (P'.mergeAppend x'6 y'6)
        (P'.mergeAppend x'7 y'7)
 
instance P'.Default FileOptions where
  defaultValue
    = FileOptions P'.defaultValue P'.defaultValue (P'.Just P'.False) (P'.Just (P'.read "CODE_SIZE")) P'.defaultValue P'.defaultValue
        P'.defaultValue
 
instance P'.Wire FileOptions where
  wireSize ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
          = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeOpt 1 14 x'4 +
               P'.wireSizeRep 2 11 x'5
               + P'.wireSizeExtField x'6
               + P'.wireSizeUnknownField x'7)
  wirePut ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7)
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
              P'.wirePutOpt 10 9 x'1
              P'.wirePutOpt 66 9 x'2
              P'.wirePutOpt 72 14 x'4
              P'.wirePutOpt 80 8 x'3
              P'.wirePutRep 7994 11 x'5
              P'.wirePutExtField x'6
              P'.wirePutUnknownField x'7
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith other'Field update'Self
        11 -> P'.getMessageWith other'Field update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{java_package = P'.Just new'Field}) (P'.wireGet 9)
              8 -> P'.fmap (\ new'Field -> old'Self{java_outer_classname = P'.Just new'Field}) (P'.wireGet 9)
              10 -> P'.fmap (\ new'Field -> old'Self{java_multiple_files = P'.Just new'Field}) (P'.wireGet 8)
              9 -> P'.fmap (\ new'Field -> old'Self{optimize_for = P'.Just new'Field}) (P'.wireGet 14)
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
 
instance P'.MessageAPI msg' (msg' -> FileOptions) FileOptions where
  getVal m' f' = f' m'
 
instance P'.GPB FileOptions
 
instance P'.ReflectDescriptor FileOptions where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"java_package\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"java_outer_classname\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"java_multiple_files\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"optimize_for\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"OptimizeMode\"}), hsRawDefault = Just (Chunk \"CODE_SIZE\" Empty), hsDefault = Just (HsDef'Enum \"CODE_SIZE\")},FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileOptions\", baseName = \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, wireTagLength = 2, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True}"