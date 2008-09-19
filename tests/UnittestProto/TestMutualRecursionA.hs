module UnittestProto.TestMutualRecursionA (TestMutualRecursionA(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.TestMutualRecursionB as UnittestProto (TestMutualRecursionB)
 
data TestMutualRecursionA = TestMutualRecursionA{bb :: P'.Maybe UnittestProto.TestMutualRecursionB}
                          deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestMutualRecursionA where
  mergeEmpty = TestMutualRecursionA P'.mergeEmpty
  mergeAppend (TestMutualRecursionA x'1) (TestMutualRecursionA y'1) = TestMutualRecursionA (P'.mergeAppend x'1 y'1)
 
instance P'.Default TestMutualRecursionA where
  defaultValue = TestMutualRecursionA P'.defaultValue
 
instance P'.Wire TestMutualRecursionA where
  wireSize ft' self'@(TestMutualRecursionA x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(TestMutualRecursionA x'1)
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
              1 -> P'.fmap (\ new'Field -> old'Self{bb = P'.mergeAppend (bb old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestMutualRecursionA) TestMutualRecursionA where
  getVal m' f' = f' m'
 
instance P'.GPB TestMutualRecursionA
 
instance P'.ReflectDescriptor TestMutualRecursionA where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestMutualRecursionA\"}, descFilePath = [\"UnittestProto\",\"TestMutualRecursionA.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestMutualRecursionA\", baseName = \"bb\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestMutualRecursionB\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"