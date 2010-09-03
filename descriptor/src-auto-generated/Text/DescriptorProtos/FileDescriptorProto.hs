module Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProtos (DescriptorProto)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.FileOptions as DescriptorProtos (FileOptions)
import qualified Text.DescriptorProtos.ServiceDescriptorProto as DescriptorProtos (ServiceDescriptorProto)
 
data FileDescriptorProto = FileDescriptorProto{name :: P'.Maybe P'.Utf8, package :: P'.Maybe P'.Utf8, dependency :: P'.Seq P'.Utf8,
                                               message_type :: P'.Seq DescriptorProtos.DescriptorProto,
                                               enum_type :: P'.Seq DescriptorProtos.EnumDescriptorProto,
                                               service :: P'.Seq DescriptorProtos.ServiceDescriptorProto,
                                               extension :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                               options :: P'.Maybe DescriptorProtos.FileOptions, unknown'field :: P'.UnknownField}
                         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage FileDescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable FileDescriptorProto where
  mergeEmpty
   = FileDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
      P'.mergeEmpty
      P'.mergeEmpty
  mergeAppend (FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (FileDescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = FileDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
 
instance P'.Default FileDescriptorProto where
  defaultValue
   = FileDescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire FileDescriptorProto where
  wireSize ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 9 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeRep 1 11 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeUnknownField x'9)
  wirePut ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 9 x'1
             P'.wirePutOpt 18 9 x'2
             P'.wirePutRep 26 9 x'3
             P'.wirePutRep 34 11 x'4
             P'.wirePutRep 42 11 x'5
             P'.wirePutRep 50 11 x'6
             P'.wirePutRep 58 11 x'7
             P'.wirePutOpt 66 11 x'8
             P'.wirePutUnknownField x'9
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
             18 -> P'.fmap (\ new'Field -> old'Self{package = P'.Just new'Field}) (P'.wireGet 9)
             26 -> P'.fmap (\ new'Field -> old'Self{dependency = P'.append (dependency old'Self) new'Field}) (P'.wireGet 9)
             34 -> P'.fmap (\ new'Field -> old'Self{message_type = P'.append (message_type old'Self) new'Field}) (P'.wireGet 11)
             42 -> P'.fmap (\ new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
             50 -> P'.fmap (\ new'Field -> old'Self{service = P'.append (service old'Self) new'Field}) (P'.wireGet 11)
             58 -> P'.fmap (\ new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
             66 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> FileDescriptorProto) FileDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB FileDescriptorProto
 
instance P'.ReflectDescriptor FileDescriptorProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42, 50, 58, 66])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"package\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.dependency\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"dependency\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.message_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"message_type\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.enum_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"enum_type\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.service\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"service\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.ServiceDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"ServiceDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.extension\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"extension\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"options\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"