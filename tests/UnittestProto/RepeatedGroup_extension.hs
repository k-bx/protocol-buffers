module UnittestProto.RepeatedGroup_extension (RepeatedGroup_extension(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data RepeatedGroup_extension = RepeatedGroup_extension{a :: P'.Maybe P'.Int32}
                             deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RepeatedGroup_extension where
  mergeEmpty = RepeatedGroup_extension P'.mergeEmpty
  mergeAppend (RepeatedGroup_extension x'1) (RepeatedGroup_extension y'1) = RepeatedGroup_extension (P'.mergeAppend x'1 y'1)
 
instance P'.Default RepeatedGroup_extension where
  defaultValue = RepeatedGroup_extension P'.defaultValue
 
instance P'.Wire RepeatedGroup_extension where
  wireSize ft' self'@(RepeatedGroup_extension x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 2 5 x'1)
  wirePut ft' self'@(RepeatedGroup_extension x'1)
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
              P'.wirePutOpt 376 5 x'1
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              47 -> P'.fmap (\ new'Field -> old'Self{a = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> RepeatedGroup_extension) RepeatedGroup_extension where
  getVal m' f' = f' m'
 
instance P'.GPB RepeatedGroup_extension
 
instance P'.ReflectDescriptor RepeatedGroup_extension where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"RepeatedGroup_extension\"}, descFilePath = [\"UnittestProto\",\"RepeatedGroup_extension.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.RepeatedGroup_extension\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 47}, wireTag = WireTag {getWireTag = 376}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"