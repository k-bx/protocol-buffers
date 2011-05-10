{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.Google.Protobuf.Compiler.CodeGeneratorResponse (CodeGeneratorResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.Google.Protobuf.Compiler.CodeGeneratorResponse.File as Google.Protobuf.Compiler.CodeGeneratorResponse (File)
 
data CodeGeneratorResponse = CodeGeneratorResponse{error :: !(P'.Maybe P'.Utf8),
                                                   file :: !(P'.Seq Google.Protobuf.Compiler.CodeGeneratorResponse.File),
                                                   unknown'field :: !P'.UnknownField}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable)
 
instance P'.UnknownMessage CodeGeneratorResponse where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable CodeGeneratorResponse where
  mergeAppend (CodeGeneratorResponse x'1 x'2 x'3) (CodeGeneratorResponse y'1 y'2 y'3)
   = CodeGeneratorResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default CodeGeneratorResponse where
  defaultValue = CodeGeneratorResponse P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire CodeGeneratorResponse where
  wireSize ft' self'@(CodeGeneratorResponse x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(CodeGeneratorResponse x'1 x'2 x'3)
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
             P'.wirePutRep 122 11 x'2
             P'.wirePutUnknownField x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{error = Prelude'.Just new'Field}) (P'.wireGet 9)
             122 -> Prelude'.fmap (\ !new'Field -> old'Self{file = P'.append (file old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> CodeGeneratorResponse) CodeGeneratorResponse where
  getVal m' f' = f' m'
 
instance P'.GPB CodeGeneratorResponse
 
instance P'.ReflectDescriptor CodeGeneratorResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 122])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.compiler.CodeGeneratorResponse\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\"], baseName = MName \"CodeGeneratorResponse\"}, descFilePath = [\"Text\",\"Google\",\"Protobuf\",\"Compiler\",\"CodeGeneratorResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorResponse.error\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\"], baseName' = FName \"error\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.compiler.CodeGeneratorResponse.file\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\"], baseName' = FName \"file\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.compiler.CodeGeneratorResponse.File\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"Google\",MName \"Protobuf\",MName \"Compiler\",MName \"CodeGeneratorResponse\"], baseName = MName \"File\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"