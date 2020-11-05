{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProtos (DescriptorProto)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.FileOptions as DescriptorProtos (FileOptions)
import qualified Text.DescriptorProtos.ServiceDescriptorProto as DescriptorProtos (ServiceDescriptorProto)
import qualified Text.DescriptorProtos.SourceCodeInfo as DescriptorProtos (SourceCodeInfo)

data FileDescriptorProto = FileDescriptorProto{name :: !(P'.Maybe P'.Utf8), package :: !(P'.Maybe P'.Utf8),
                                               dependency :: !(P'.Seq P'.Utf8), public_dependency :: !(P'.Seq P'.Int32),
                                               weak_dependency :: !(P'.Seq P'.Int32),
                                               message_type :: !(P'.Seq DescriptorProtos.DescriptorProto),
                                               enum_type :: !(P'.Seq DescriptorProtos.EnumDescriptorProto),
                                               service :: !(P'.Seq DescriptorProtos.ServiceDescriptorProto),
                                               extension :: !(P'.Seq DescriptorProtos.FieldDescriptorProto),
                                               options :: !(P'.Maybe DescriptorProtos.FileOptions),
                                               source_code_info :: !(P'.Maybe DescriptorProtos.SourceCodeInfo),
                                               syntax :: !(P'.Maybe P'.Utf8), unknown'field :: !(P'.UnknownField)}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage FileDescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable FileDescriptorProto where
  mergeAppend (FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
   (FileDescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13)
   = FileDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)

instance P'.Default FileDescriptorProto where
  defaultValue
   = FileDescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire FileDescriptorProto where
  wireSize ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 9 x'3 + P'.wireSizeRep 1 5 x'4 +
             P'.wireSizeRep 1 5 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeRep 1 11 x'7
             + P'.wireSizeRep 1 11 x'8
             + P'.wireSizeRep 1 11 x'9
             + P'.wireSizeOpt 1 11 x'10
             + P'.wireSizeOpt 1 11 x'11
             + P'.wireSizeOpt 1 9 x'12
             + P'.wireSizeUnknownField x'13)
  wirePutWithSize ft' self'@(FileDescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutOptWithSize 18 9 x'2, P'.wirePutRepWithSize 26 9 x'3,
             P'.wirePutRepWithSize 34 11 x'6, P'.wirePutRepWithSize 42 11 x'7, P'.wirePutRepWithSize 50 11 x'8,
             P'.wirePutRepWithSize 58 11 x'9, P'.wirePutOptWithSize 66 11 x'10, P'.wirePutOptWithSize 74 11 x'11,
             P'.wirePutRepWithSize 80 5 x'4, P'.wirePutRepWithSize 88 5 x'5, P'.wirePutOptWithSize 98 9 x'12,
             P'.wirePutUnknownFieldWithSize x'13]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.loadUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{package = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{dependency = P'.append (dependency old'Self) new'Field}) (P'.wireGet 9)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{public_dependency = P'.append (public_dependency old'Self) new'Field})
                    (P'.wireGet 5)
             82 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{public_dependency = P'.mergeAppend (public_dependency old'Self) new'Field})
                    (P'.wireGetPacked 5)
             88 -> Prelude'.fmap (\ !new'Field -> old'Self{weak_dependency = P'.append (weak_dependency old'Self) new'Field})
                    (P'.wireGet 5)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{weak_dependency = P'.mergeAppend (weak_dependency old'Self) new'Field})
                    (P'.wireGetPacked 5)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{message_type = P'.append (message_type old'Self) new'Field})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{service = P'.append (service old'Self) new'Field}) (P'.wireGet 11)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             74 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{source_code_info = P'.mergeAppend (source_code_info old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             98 -> Prelude'.fmap (\ !new'Field -> old'Self{syntax = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> FileDescriptorProto) FileDescriptorProto where
  getVal m' f' = f' m'

instance P'.GPB FileDescriptorProto

instance P'.ReflectDescriptor FileDescriptorProto where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42, 50, 58, 66, 74, 80, 82, 88, 90, 98])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.dependency\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"dependency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.public_dependency\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"public_dependency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Just (WireTag {getWireTag = 80},WireTag {getWireTag = 82}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.weak_dependency\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"weak_dependency\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 88}, packedTag = Just (WireTag {getWireTag = 88},WireTag {getWireTag = 90}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.message_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"message_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.enum_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"enum_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.service\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"service\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.ServiceDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"ServiceDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.extension\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"extension\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"options\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.source_code_info\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"source_code_info\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.SourceCodeInfo\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"SourceCodeInfo\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorProto.syntax\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorProto\"], baseName' = FName \"syntax\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 98}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType FileDescriptorProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg FileDescriptorProto where
  textPut msg
   = do
       P'.tellT "name" (name msg)
       P'.tellT "package" (package msg)
       P'.tellT "dependency" (dependency msg)
       P'.tellT "public_dependency" (public_dependency msg)
       P'.tellT "weak_dependency" (weak_dependency msg)
       P'.tellT "message_type" (message_type msg)
       P'.tellT "enum_type" (enum_type msg)
       P'.tellT "service" (service msg)
       P'.tellT "extension" (extension msg)
       P'.tellT "options" (options msg)
       P'.tellT "source_code_info" (source_code_info msg)
       P'.tellT "syntax" (syntax msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'name, parse'package, parse'dependency, parse'public_dependency, parse'weak_dependency, parse'message_type,
                   parse'enum_type, parse'service, parse'extension, parse'options, parse'source_code_info, parse'syntax])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'package
         = P'.try
            (do
               v <- P'.getT "package"
               Prelude'.return (\ o -> o{package = v}))
        parse'dependency
         = P'.try
            (do
               v <- P'.getT "dependency"
               Prelude'.return (\ o -> o{dependency = P'.append (dependency o) v}))
        parse'public_dependency
         = P'.try
            (do
               v <- P'.getT "public_dependency"
               Prelude'.return (\ o -> o{public_dependency = P'.append (public_dependency o) v}))
        parse'weak_dependency
         = P'.try
            (do
               v <- P'.getT "weak_dependency"
               Prelude'.return (\ o -> o{weak_dependency = P'.append (weak_dependency o) v}))
        parse'message_type
         = P'.try
            (do
               v <- P'.getT "message_type"
               Prelude'.return (\ o -> o{message_type = P'.append (message_type o) v}))
        parse'enum_type
         = P'.try
            (do
               v <- P'.getT "enum_type"
               Prelude'.return (\ o -> o{enum_type = P'.append (enum_type o) v}))
        parse'service
         = P'.try
            (do
               v <- P'.getT "service"
               Prelude'.return (\ o -> o{service = P'.append (service o) v}))
        parse'extension
         = P'.try
            (do
               v <- P'.getT "extension"
               Prelude'.return (\ o -> o{extension = P'.append (extension o) v}))
        parse'options
         = P'.try
            (do
               v <- P'.getT "options"
               Prelude'.return (\ o -> o{options = v}))
        parse'source_code_info
         = P'.try
            (do
               v <- P'.getT "source_code_info"
               Prelude'.return (\ o -> o{source_code_info = v}))
        parse'syntax
         = P'.try
            (do
               v <- P'.getT "syntax"
               Prelude'.return (\ o -> o{syntax = v}))