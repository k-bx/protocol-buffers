module UnittestProto.TestNestedMessageHasBits.NestedMessage (NestedMessage(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.ForeignMessage as UnittestProto (ForeignMessage)
 
data NestedMessage = NestedMessage{nestedmessage_repeated_int32 :: P'.Seq P'.Int32,
                                   nestedmessage_repeated_foreignmessage :: P'.Seq UnittestProto.ForeignMessage}
                   deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
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
        calc'Size = (P'.wireSizeRep 1 5 x'1 + P'.wireSizeRep 1 11 x'2)
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
              P'.wirePutRep 8 5 x'1
              P'.wirePutRep 18 11 x'2
  wireGet ft'
    = case ft' of
        10 -> P'.getBareMessage update'Self
        11 -> P'.getMessage update'Self
        _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
          = case field'Number of
              1 -> P'.fmap
                     (\ new'Field ->
                        old'Self{nestedmessage_repeated_int32 = P'.append (nestedmessage_repeated_int32 old'Self) new'Field})
                     (P'.wireGet 5)
              2 -> P'.fmap
                     (\ new'Field ->
                        old'Self{nestedmessage_repeated_foreignmessage =
                                   P'.append (nestedmessage_repeated_foreignmessage old'Self) new'Field})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> NestedMessage) NestedMessage where
  getVal m' f' = f' m'
 
instance P'.GPB NestedMessage
 
instance P'.ReflectDescriptor NestedMessage where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestNestedMessageHasBits\", baseName = \"NestedMessage\"}, descFilePath = [\"UnittestProto\",\"TestNestedMessageHasBits\",\"NestedMessage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestNestedMessageHasBits.NestedMessage\", baseName = \"nestedmessage_repeated_int32\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestNestedMessageHasBits.NestedMessage\", baseName = \"nestedmessage_repeated_foreignmessage\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"ForeignMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"