module UnittestProto.TestForeignNested (TestForeignNested(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified UnittestProto.TestAllTypes.NestedMessage as UnittestProto.TestAllTypes (NestedMessage)
 
data TestForeignNested = TestForeignNested{foreign_nested :: P'.Maybe UnittestProto.TestAllTypes.NestedMessage}
                       deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable TestForeignNested where
  mergeEmpty = TestForeignNested P'.mergeEmpty
  mergeAppend (TestForeignNested x'1) (TestForeignNested y'1) = TestForeignNested (P'.mergeAppend x'1 y'1)
 
instance P'.Default TestForeignNested where
  defaultValue = TestForeignNested (P'.Just P'.defaultValue)
 
instance P'.Wire TestForeignNested where
  wireSize ft' self'@(TestForeignNested x'1)
    = case ft' of
        10 -> calc'Size
        11 -> P'.prependMessageSize calc'Size
        _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePut ft' self'@(TestForeignNested x'1)
    = case ft' of
        10 -> put'Fields
        11
          -> do
               P'.putSize (P'.wireSize 11 self')
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
              1 -> P'.fmap (\ new'Field -> old'Self{foreign_nested = P'.mergeAppend (foreign_nested old'Self) (P'.Just new'Field)})
                     (P'.wireGet 11)
              _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> TestForeignNested) TestForeignNested where
  getVal m' f' = f' m'
 
instance P'.GPB TestForeignNested
 
instance P'.ReflectDescriptor TestForeignNested where
  reflectDescriptorInfo _
    = P'.read
        "DescriptorInfo {descName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto\", baseName = \"TestForeignNested\"}, descFilePath = [\"UnittestProto\",\"TestForeignNested.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestForeignNested\", baseName = \"foreign_nested\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {haskellPrefix = \"\", parentModule = \"UnittestProto.TestAllTypes\", baseName = \"NestedMessage\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList []}"