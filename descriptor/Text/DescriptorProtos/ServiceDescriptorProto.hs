module Text.DescriptorProtos.ServiceDescriptorProto (ServiceDescriptorProto(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.MethodDescriptorProto as DescriptorProtos (MethodDescriptorProto)
import qualified Text.DescriptorProtos.ServiceOptions as DescriptorProtos (ServiceOptions)
 
data ServiceDescriptorProto = ServiceDescriptorProto{name :: P'.Maybe P'.Utf8,
                                                     method :: P'.Seq DescriptorProtos.MethodDescriptorProto,
                                                     options :: P'.Maybe DescriptorProtos.ServiceOptions,
                                                     unknown'field :: P'.UnknownField}
                            deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.UnknownMessage ServiceDescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable ServiceDescriptorProto where
  mergeEmpty = ServiceDescriptorProto P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (ServiceDescriptorProto x'1 x'2 x'3 x'4) (ServiceDescriptorProto y'1 y'2 y'3 y'4)
   = ServiceDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default ServiceDescriptorProto where
  defaultValue = ServiceDescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ServiceDescriptorProto where
  wireSize ft' self'@(ServiceDescriptorProto x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(ServiceDescriptorProto x'1 x'2 x'3 x'4)
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
             P'.wirePutRep 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutUnknownField x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith check'allowed
       11 -> P'.getMessageWith check'allowed
       _ -> P'.wireGetErr ft'
    where
        update'Self field'Number old'Self
         = case field'Number of
             1 -> P'.fmap (\ new'Field -> old'Self{name = P'.Just new'Field}) (P'.wireGet 9)
             2 -> P'.fmap (\ new'Field -> old'Self{method = P'.append (method old'Self) new'Field}) (P'.wireGet 11)
             3 -> P'.fmap (\ new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
             _ -> P'.unknownField old'Self field'Number
        allowed'wire'Tags = P'.fromDistinctAscList [10, 18, 26]
        check'allowed wire'Tag field'Number wire'Type old'Self
         = P'.catchError
            (if P'.member wire'Tag allowed'wire'Tags then update'Self field'Number old'Self else
              P'.unknown field'Number wire'Type old'Self)
            (\ _ -> P'.loadUnknown field'Number wire'Type old'Self)
 
instance P'.MessageAPI msg' (msg' -> ServiceDescriptorProto) ServiceDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB ServiceDescriptorProto
 
instance P'.ReflectDescriptor ServiceDescriptorProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.ServiceDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"ServiceDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"ServiceDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.ServiceDescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"ServiceDescriptorProto\"], baseName' = FName \"name\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.ServiceDescriptorProto.method\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"ServiceDescriptorProto\"], baseName' = FName \"method\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, wireTagLength = 1, isRequired = False, canRepeat = True, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.MethodDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"MethodDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.ServiceDescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"ServiceDescriptorProto\"], baseName' = FName \"options\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, wireTagLength = 1, isRequired = False, canRepeat = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.ServiceOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"ServiceOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True}"