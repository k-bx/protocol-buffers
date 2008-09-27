module UnittestProto.TestAllTypes.NestedMessage (NestedMessage(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data NestedMessage = NestedMessage{bb :: P'.Maybe P'.Int32, unknown'field :: P'.UnknownField}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage NestedMessage where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable NestedMessage where
  mergeEmpty = NestedMessage P'.mergeEmpty P'.mergeEmpty
  mergeAppend (NestedMessage x'1 x'2) (NestedMessage y'1 y'2) = NestedMessage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default NestedMessage where
  defaultValue = NestedMessage P'.defaultValue P'.defaultValue
 
instance P'.Wire NestedMessage where
  wireSize ft' self'@(NestedMessage x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(NestedMessage x'1 x'2)
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
              P'.wirePutOpt 8 5 x'1
              P'.wirePutUnknownField x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessageWith P'.loadUnknown update'Self
        11 -> P'.getMessageWith P'.loadUnknown update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap (\ new'Field -> old'Self{bb = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> NestedMessage) NestedMessage where
  getVal m' f' = f' m'
 
instance P'.GPB NestedMessage
 
instance P'.ReflectDescriptor NestedMessage where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedMessage\"}, descFilePath = [\"UnittestProto\",\"TestAllTypes\",\"NestedMessage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes.NestedMessage\", baseName = \"bb\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"