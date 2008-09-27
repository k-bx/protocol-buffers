module UnittestProto.ForeignMessage (ForeignMessage(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ForeignMessage = ForeignMessage{c :: P'.Maybe P'.Int32, unknown'field :: P'.UnknownField}
                    deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage ForeignMessage where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable ForeignMessage where
  mergeEmpty = ForeignMessage P'.mergeEmpty P'.mergeEmpty
  mergeAppend (ForeignMessage x'1 x'2) (ForeignMessage y'1 y'2) = ForeignMessage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ForeignMessage where
  defaultValue = ForeignMessage P'.defaultValue P'.defaultValue
 
instance P'.Wire ForeignMessage where
  wireSize ft' self'@(ForeignMessage x'1 x'2)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(ForeignMessage x'1 x'2)
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
              1 -> P'.fmap (\ new'Field -> old'Self{c = P'.Just new'Field}) (P'.wireGet 5)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> ForeignMessage) ForeignMessage where
  getVal m' f' = f' m'
 
instance P'.GPB ForeignMessage
 
instance P'.ReflectDescriptor ForeignMessage where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}, descFilePath = [\"UnittestProto\",\"ForeignMessage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.ForeignMessage\", baseName = \"c\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"