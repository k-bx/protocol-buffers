module UnittestProto.TestAllTypes.RepeatedGroup (RepeatedGroup(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data RepeatedGroup = RepeatedGroup{a :: P'.Maybe P'.Int32}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RepeatedGroup where
  mergeEmpty = RepeatedGroup P'.mergeEmpty
  mergeAppend (RepeatedGroup x'1) (RepeatedGroup y'1) = RepeatedGroup (P'.mergeAppend x'1 y'1)
 
instance P'.Default RepeatedGroup where
  defaultValue = RepeatedGroup P'.defaultValue
 
instance P'.Wire RepeatedGroup where
  wireSize ft' self'@(RepeatedGroup x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 2 5 x'1)
  wirePut ft' self'@(RepeatedGroup x'1)
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
 
instance P'.MessageAPI msg' (msg' -> RepeatedGroup) RepeatedGroup where
  getVal m' f' = f' m'
 
instance P'.GPB RepeatedGroup
 
instance P'.ReflectDescriptor RepeatedGroup where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"RepeatedGroup\"}, descFilePath = [\"UnittestProto\",\"TestAllTypes\",\"RepeatedGroup.hs\"], isGroup = True, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes.RepeatedGroup\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 47}, wireTag = WireTag {getWireTag = 376}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"