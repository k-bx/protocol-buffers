module Text.Google.Protobuf.Compiler.CodeGeneratorResponse.File (File(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data File = File{name :: P'.Maybe P'.Utf8, insertion_point :: P'.Maybe P'.Utf8, content :: P'.Maybe P'.Utf8}
          deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable File where
  mergeEmpty = File P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (File x'1 x'2 x'3) (File y'1 y'2 y'3) = File (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
                                                       (P'.mergeAppend x'3 y'3)
 
instance P'.Default File where
  defaultValue = File P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire File where
  wireSize ft' self'@(File x'1 x'2 x'3) = case ft' of
                                            10 -> calc'Size
                                            11 -> P'.prependMessageSize calc'Size
                                            _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 9 x'3)
  wirePut ft' self'@(File x'1 x'2 x'3) = case ft' of
                                           10 -> put'Fields
                                           11 -> do
                                                   P'.putSize (P'.wireSize 10 self')
                                                   put'Fields
                                           _ -> P'.wirePutErr ft' self'
    where
        put'Fields = do
                       P'.wirePutOpt 10 9 x'1
                       P'.wirePutOpt 18 9 x'2
                       P'.wirePutOpt 122 9 x'3
  wireGet ft' = case ft' of
                  10 -> P'.getBareMessageWith update'Self
                  11 -> P'.getMessageWith update'Self
                  _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self = case wire'Tag of
                                          10 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
                                          18 -> P'.fmap (\ new'Field -> old'Self{insertion_point = P'.Just new'Field})
                                                 (P'.wireGet 9)
                                          122 -> P'.fmap (\ new'Field -> old'Self{content = P'.Just new'Field}) (P'.wireGet 9)
                                          _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                                                P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> File) File where
  getVal m' f' = f' m'
 
instance P'.GPB File
 
instance P'.ReflectDescriptor File where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 122])
  reflectDescriptorInfo _ = P'.read
                             "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.compiler.CodeGeneratorResponse.File\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\"], baseName = MName \"File\"}, descFilePath = [\"Text\",\"Google\",\"Protobuf\",\"Compiler\",\"CodeGeneratorResponse\",\"File.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorResponse.File.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\",MName \"File\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorResponse.File.insertion_point\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\",MName \"File\"], baseName' = FName \"insertion_point\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorResponse.File.content\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\",MName \"File\"], baseName' = FName \"content\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"