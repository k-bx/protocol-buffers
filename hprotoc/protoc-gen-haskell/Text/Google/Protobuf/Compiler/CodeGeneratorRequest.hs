{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.Google.Protobuf.Compiler.CodeGeneratorRequest (CodeGeneratorRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileDescriptorProto as DescriptorProtos (FileDescriptorProto)
 
data CodeGeneratorRequest = CodeGeneratorRequest{file_to_generate :: !(P'.Seq P'.Utf8), parameter :: !(P'.Maybe P'.Utf8),
                                                 proto_file :: !(P'.Seq DescriptorProtos.FileDescriptorProto),
                                                 unknown'field :: !P'.UnknownField}
                          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable)
 
instance P'.UnknownMessage CodeGeneratorRequest where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable CodeGeneratorRequest where
  mergeAppend (CodeGeneratorRequest x'1 x'2 x'3 x'4) (CodeGeneratorRequest y'1 y'2 y'3 y'4)
   = CodeGeneratorRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default CodeGeneratorRequest where
  defaultValue = CodeGeneratorRequest P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire CodeGeneratorRequest where
  wireSize ft' self'@(CodeGeneratorRequest x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeUnknownField x'4)
  wirePut ft' self'@(CodeGeneratorRequest x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 9 x'1
             P'.wirePutOpt 18 9 x'2
             P'.wirePutRep 122 11 x'3
             P'.wirePutUnknownField x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{file_to_generate = P'.append (file_to_generate old'Self) new'Field})
                    (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{parameter = Prelude'.Just new'Field}) (P'.wireGet 9)
             122 -> Prelude'.fmap (\ !new'Field -> old'Self{proto_file = P'.append (proto_file old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> CodeGeneratorRequest) CodeGeneratorRequest where
  getVal m' f' = f' m'
 
instance P'.GPB CodeGeneratorRequest
 
instance P'.ReflectDescriptor CodeGeneratorRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 122])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.compiler.CodeGeneratorRequest\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\"], baseName = MName \"CodeGeneratorRequest\"}, descFilePath = [\"Text\",\"Google\",\"Protobuf\",\"Compiler\",\"CodeGeneratorRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorRequest.file_to_generate\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorRequest\"], baseName' = FName \"file_to_generate\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorRequest.parameter\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorRequest\"], baseName' = FName \"parameter\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorRequest.proto_file\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorRequest\"], baseName' = FName \"proto_file\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"