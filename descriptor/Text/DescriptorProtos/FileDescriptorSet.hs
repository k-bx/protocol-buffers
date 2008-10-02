module Text.DescriptorProtos.FileDescriptorSet (FileDescriptorSet(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileDescriptorProto as DescriptorProtos (FileDescriptorProto)
 
data FileDescriptorSet = FileDescriptorSet{file :: P'.Seq DescriptorProtos.FileDescriptorProto}
                       deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable FileDescriptorSet where
  mergeEmpty = FileDescriptorSet P'.mergeEmpty
  mergeAppend (FileDescriptorSet x'1) (FileDescriptorSet y'1) = FileDescriptorSet (P'.mergeAppend x'1 y'1)
 
instance P'.Default FileDescriptorSet where
  defaultValue = FileDescriptorSet P'.defaultValue
 
instance P'.Wire FileDescriptorSet where
  wireSize ft' self'@(FileDescriptorSet x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(FileDescriptorSet x'1)
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
              P'.wirePutRep 10 11 x'1
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.unknown update'Self
        11 -> P'.getMessageWith P'.unknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{file = P'.append (file old'Self) new'Field}) (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> FileDescriptorSet) FileDescriptorSet where
  getVal m' f' = f' m'
 
instance P'.GPB FileDescriptorSet
 
instance P'.ReflectDescriptor FileDescriptorSet where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileDescriptorSet\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileDescriptorSet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos.FileDescriptorSet\", baseName = \"file\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"Text\", parentModule = \"DescriptorProtos\", baseName = \"FileDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"