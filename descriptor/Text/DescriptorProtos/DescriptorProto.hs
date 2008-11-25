module Text.DescriptorProtos.DescriptorProto (DescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as DescriptorProtos.DescriptorProto (ExtensionRange)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.MessageOptions as DescriptorProtos (MessageOptions)
 
data DescriptorProto = DescriptorProto{name :: P'.Maybe P'.Utf8, field :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                       extension :: P'.Seq DescriptorProtos.FieldDescriptorProto,
                                       nested_type :: P'.Seq DescriptorProto,
                                       enum_type :: P'.Seq DescriptorProtos.EnumDescriptorProto,
                                       extension_range :: P'.Seq DescriptorProtos.DescriptorProto.ExtensionRange,
                                       options :: P'.Maybe DescriptorProtos.MessageOptions, unknown'field :: P'.UnknownField}
                     deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage DescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable DescriptorProto where
  mergeEmpty
   = DescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (DescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
   = DescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
 
instance P'.Default DescriptorProto where
  defaultValue
   = DescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
 
instance P'.Wire DescriptorProto where
  wireSize ft' self'@(DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeOpt 1 11 x'7
             + P'.wireSizeUnknownField x'8)
  wirePut ft' self'@(DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
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
             P'.wirePutRep 26 11 x'4
             P'.wirePutRep 34 11 x'5
             P'.wirePutRep 42 11 x'6
             P'.wirePutRep 50 11 x'3
             P'.wirePutOpt 58 11 x'7
             P'.wirePutUnknownField x'8
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith P'.loadUnknown update'Self
       11 -> P'.getMessageWith P'.loadUnknown update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
         = case field'Number of
             1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
             2 -> P'.fmap (\ new'Field -> old'Self{field = P'.append (field old'Self) new'Field}) (P'.wireGet 11)
             6 -> P'.fmap (\ new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
             3 -> P'.fmap (\ new'Field -> old'Self{nested_type = P'.append (nested_type old'Self) new'Field}) (P'.wireGet 11)
             4 -> P'.fmap (\ new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
             5
              -> P'.fmap (\ new'Field -> old'Self{extension_range = P'.append (extension_range old'Self) new'Field}) (P'.wireGet 11)
             7 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
             _ -> P'.unknownField field'Number
 
instance P'.MessageAPI msg' (msg' -> DescriptorProto) DescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB DescriptorProto
 
instance P'.ReflectDescriptor DescriptorProto where
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"DescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.field\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"field\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.extension\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"extension\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.nested_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"nested_type\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.enum_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"enum_type\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.extension_range\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"extension_range\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto.ExtensionRange\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName = MName \"ExtensionRange\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"options\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.MessageOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"MessageOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42, 50, 58])