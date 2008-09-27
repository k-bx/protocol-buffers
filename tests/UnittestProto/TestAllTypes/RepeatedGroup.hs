module UnittestProto.TestAllTypes.RepeatedGroup (RepeatedGroup(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data RepeatedGroup = RepeatedGroup{a :: P'.Maybe P'.Int32, unknown'field :: P'.UnknownField}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage RepeatedGroup where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable RepeatedGroup where
  mergeEmpty = RepeatedGroup P'.mergeEmpty P'.mergeEmpty
  mergeAppend (RepeatedGroup x'1 x'2) (RepeatedGroup y'1 y'2) = RepeatedGroup (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default RepeatedGroup where
  defaultValue = RepeatedGroup P'.defaultValue P'.defaultValue
 
instance P'.Wire RepeatedGroup where
  wireSize ft' self'@(RepeatedGroup x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 2 5 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(RepeatedGroup x'1 x'2)
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
              P'.wirePutUnknownField x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
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
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"RepeatedGroup\"}, descFilePath = [\"UnittestProto\",\"TestAllTypes\",\"RepeatedGroup.hs\"], isGroup = True, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes.RepeatedGroup\", baseName = \"a\"}, fieldNumber = FieldId {getFieldId = 47}, wireTag = WireTag {getWireTag = 376}, wireTagLength = 2, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"