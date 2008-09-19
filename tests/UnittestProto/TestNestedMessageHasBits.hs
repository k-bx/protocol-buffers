module UnittestProto.TestNestedMessageHasBits (TestNestedMessageHasBits(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.TestNestedMessageHasBits.NestedMessage as UnittestProto.TestNestedMessageHasBits (NestedMessage)
 
data TestNestedMessageHasBits = TestNestedMessageHasBits{optional_nested_message ::
                                                         P'.Maybe UnittestProto.TestNestedMessageHasBits.NestedMessage}
                              deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestNestedMessageHasBits where
  mergeEmpty = TestNestedMessageHasBits P'.mergeEmpty
  mergeAppend (TestNestedMessageHasBits x'1) (TestNestedMessageHasBits y'1) = TestNestedMessageHasBits (P'.mergeAppend x'1 y'1)
 
instance P'.Default TestNestedMessageHasBits where
  defaultValue = TestNestedMessageHasBits P'.defaultValue
 
instance P'.Wire TestNestedMessageHasBits where
  wireSize ft' self'@(TestNestedMessageHasBits x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(TestNestedMessageHasBits x'1)
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
              P'.wirePutOpt 10 11 x'1
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
                        old'Self{optional_nested_message = P'.mergeAppend (optional_nested_message old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestNestedMessageHasBits) TestNestedMessageHasBits where
  getVal m' f' = f' m'
 
instance P'.GPB TestNestedMessageHasBits
 
instance P'.ReflectDescriptor TestNestedMessageHasBits where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestNestedMessageHasBits\"}, descFilePath = [\"UnittestProto\",\"TestNestedMessageHasBits.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestNestedMessageHasBits\", baseName = \"optional_nested_message\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestNestedMessageHasBits\", baseName = \"NestedMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"