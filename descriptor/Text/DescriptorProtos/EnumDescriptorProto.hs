module Text.DescriptorProtos.EnumDescriptorProto (EnumDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.EnumOptions as DescriptorProtos (EnumOptions)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto as DescriptorProtos (EnumValueDescriptorProto)
 
data EnumDescriptorProto = EnumDescriptorProto{name :: P'.Maybe P'.Utf8, value :: P'.Seq DescriptorProtos.EnumValueDescriptorProto,
                                               options :: P'.Maybe DescriptorProtos.EnumOptions, unknown'field :: P'.UnknownField}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage EnumDescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable EnumDescriptorProto where
  mergeEmpty = EnumDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (EnumDescriptorProto x'1 x'2 x'3 x'4) (EnumDescriptorProto y'1 y'2 y'3 y'4)
   = EnumDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default EnumDescriptorProto where
  defaultValue = EnumDescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EnumDescriptorProto where
  wireSize ft' self'@(EnumDescriptorProto x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(EnumDescriptorProto x'1 x'2 x'3 x'4)
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
             P'.wirePutOpt 10 9 x'1
             P'.wirePutRep 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutUnknownField x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith P'.loadUnknown update'Self
       11 -> P'.getMessageWith P'.loadUnknown update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
         = case field'Number of
             1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
             2 -> P'.fmap (\ new'Field -> old'Self{value = P'.append (value old'Self) new'Field}) (P'.wireGet 11)
             3 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
             _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> EnumDescriptorProto) EnumDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB EnumDescriptorProto
 
instance P'.ReflectDescriptor EnumDescriptorProto where
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.EnumDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"EnumDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.EnumDescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"EnumDescriptorProto\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.EnumDescriptorProto.value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"EnumDescriptorProto\"], baseName' = FName \"value\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumValueDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumValueDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.EnumDescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"EnumDescriptorProto\"], baseName' = FName \"options\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])