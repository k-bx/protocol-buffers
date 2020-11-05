{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.FileOptions (FileOptions(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions (OptimizeMode)
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)

data FileOptions = FileOptions{java_package :: !(P'.Maybe P'.Utf8), java_outer_classname :: !(P'.Maybe P'.Utf8),
                               java_multiple_files :: !(P'.Maybe P'.Bool), java_generate_equals_and_hash :: !(P'.Maybe P'.Bool),
                               java_string_check_utf8 :: !(P'.Maybe P'.Bool),
                               optimize_for :: !(P'.Maybe DescriptorProtos.FileOptions.OptimizeMode),
                               go_package :: !(P'.Maybe P'.Utf8), cc_generic_services :: !(P'.Maybe P'.Bool),
                               java_generic_services :: !(P'.Maybe P'.Bool), py_generic_services :: !(P'.Maybe P'.Bool),
                               deprecated :: !(P'.Maybe P'.Bool), cc_enable_arenas :: !(P'.Maybe P'.Bool),
                               objc_class_prefix :: !(P'.Maybe P'.Utf8), csharp_namespace :: !(P'.Maybe P'.Utf8),
                               javanano_use_deprecated_package :: !(P'.Maybe P'.Bool),
                               uninterpreted_option :: !(P'.Seq DescriptorProtos.UninterpretedOption), ext'field :: !(P'.ExtField),
                               unknown'field :: !(P'.UnknownField)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage FileOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage FileOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable FileOptions where
  mergeAppend (FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18)
   (FileOptions y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16 y'17 y'18)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
         !z'7 = P'.mergeAppend x'7 y'7
         !z'8 = P'.mergeAppend x'8 y'8
         !z'9 = P'.mergeAppend x'9 y'9
         !z'10 = P'.mergeAppend x'10 y'10
         !z'11 = P'.mergeAppend x'11 y'11
         !z'12 = P'.mergeAppend x'12 y'12
         !z'13 = P'.mergeAppend x'13 y'13
         !z'14 = P'.mergeAppend x'14 y'14
         !z'15 = P'.mergeAppend x'15 y'15
         !z'16 = P'.mergeAppend x'16 y'16
         !z'17 = P'.mergeAppend x'17 y'17
         !z'18 = P'.mergeAppend x'18 y'18
      in FileOptions z'1 z'2 z'3 z'4 z'5 z'6 z'7 z'8 z'9 z'10 z'11 z'12 z'13 z'14 z'15 z'16 z'17 z'18

instance P'.Default FileOptions where
  defaultValue
   = FileOptions P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just (Prelude'.read "SPEED"))
      P'.defaultValue
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire FileOptions where
  wireSize ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeOpt 2 8 x'4 +
             P'.wireSizeOpt 2 8 x'5
             + P'.wireSizeOpt 1 14 x'6
             + P'.wireSizeOpt 1 9 x'7
             + P'.wireSizeOpt 2 8 x'8
             + P'.wireSizeOpt 2 8 x'9
             + P'.wireSizeOpt 2 8 x'10
             + P'.wireSizeOpt 2 8 x'11
             + P'.wireSizeOpt 2 8 x'12
             + P'.wireSizeOpt 2 9 x'13
             + P'.wireSizeOpt 2 9 x'14
             + P'.wireSizeOpt 2 8 x'15
             + P'.wireSizeRep 2 11 x'16
             + P'.wireSizeExtField x'17
             + P'.wireSizeUnknownField x'18)
  wirePutWithSize ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16 x'17 x'18)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutOptWithSize 66 9 x'2, P'.wirePutOptWithSize 72 14 x'6,
             P'.wirePutOptWithSize 80 8 x'3, P'.wirePutOptWithSize 90 9 x'7, P'.wirePutOptWithSize 128 8 x'8,
             P'.wirePutOptWithSize 136 8 x'9, P'.wirePutOptWithSize 144 8 x'10, P'.wirePutOptWithSize 160 8 x'4,
             P'.wirePutOptWithSize 184 8 x'11, P'.wirePutOptWithSize 216 8 x'5, P'.wirePutOptWithSize 248 8 x'12,
             P'.wirePutOptWithSize 290 9 x'13, P'.wirePutOptWithSize 298 9 x'14, P'.wirePutOptWithSize 304 8 x'15,
             P'.wirePutRepWithSize 7994 11 x'16, P'.wirePutExtFieldWithSize x'17, P'.wirePutUnknownFieldWithSize x'18]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{java_package = Prelude'.Just new'Field}) (P'.wireGet 9)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{java_outer_classname = Prelude'.Just new'Field}) (P'.wireGet 9)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{java_multiple_files = Prelude'.Just new'Field}) (P'.wireGet 8)
             160 -> Prelude'.fmap (\ !new'Field -> old'Self{java_generate_equals_and_hash = Prelude'.Just new'Field}) (P'.wireGet 8)
             216 -> Prelude'.fmap (\ !new'Field -> old'Self{java_string_check_utf8 = Prelude'.Just new'Field}) (P'.wireGet 8)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{optimize_for = Prelude'.Just new'Field}) (P'.wireGet 14)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{go_package = Prelude'.Just new'Field}) (P'.wireGet 9)
             128 -> Prelude'.fmap (\ !new'Field -> old'Self{cc_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
             136 -> Prelude'.fmap (\ !new'Field -> old'Self{java_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
             144 -> Prelude'.fmap (\ !new'Field -> old'Self{py_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
             184 -> Prelude'.fmap (\ !new'Field -> old'Self{deprecated = Prelude'.Just new'Field}) (P'.wireGet 8)
             248 -> Prelude'.fmap (\ !new'Field -> old'Self{cc_enable_arenas = Prelude'.Just new'Field}) (P'.wireGet 8)
             290 -> Prelude'.fmap (\ !new'Field -> old'Self{objc_class_prefix = Prelude'.Just new'Field}) (P'.wireGet 9)
             298 -> Prelude'.fmap (\ !new'Field -> old'Self{csharp_namespace = Prelude'.Just new'Field}) (P'.wireGet 9)
             304 -> Prelude'.fmap (\ !new'Field -> old'Self{javanano_use_deprecated_package = Prelude'.Just new'Field})
                     (P'.wireGet 8)
             7994 -> Prelude'.fmap
                      (\ !new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                      (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> FileOptions) FileOptions where
  getVal m' f' = f' m'

instance P'.GPB FileOptions

instance P'.ReflectDescriptor FileOptions where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList [10, 66, 72, 80, 90, 128, 136, 144, 160, 184, 216, 248, 290, 298, 304, 7994])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_outer_classname\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_outer_classname\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_multiple_files\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_multiple_files\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generate_equals_and_hash\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generate_equals_and_hash\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_string_check_utf8\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_string_check_utf8\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 27}, wireTag = WireTag {getWireTag = 216}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.optimize_for\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"optimize_for\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileOptions.OptimizeMode\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName = MName \"OptimizeMode\"}), hsRawDefault = Just \"SPEED\", hsDefault = Just (HsDef'Enum \"SPEED\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.go_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"go_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.cc_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"cc_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.py_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"py_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.deprecated\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"deprecated\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.cc_enable_arenas\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"cc_enable_arenas\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 31}, wireTag = WireTag {getWireTag = 248}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.objc_class_prefix\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"objc_class_prefix\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 36}, wireTag = WireTag {getWireTag = 290}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.csharp_namespace\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"csharp_namespace\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 37}, wireTag = WireTag {getWireTag = 298}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.javanano_use_deprecated_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"javanano_use_deprecated_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 38}, wireTag = WireTag {getWireTag = 304}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"uninterpreted_option\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType FileOptions where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg FileOptions where
  textPut msg
   = do
       P'.tellT "java_package" (java_package msg)
       P'.tellT "java_outer_classname" (java_outer_classname msg)
       P'.tellT "java_multiple_files" (java_multiple_files msg)
       P'.tellT "java_generate_equals_and_hash" (java_generate_equals_and_hash msg)
       P'.tellT "java_string_check_utf8" (java_string_check_utf8 msg)
       P'.tellT "optimize_for" (optimize_for msg)
       P'.tellT "go_package" (go_package msg)
       P'.tellT "cc_generic_services" (cc_generic_services msg)
       P'.tellT "java_generic_services" (java_generic_services msg)
       P'.tellT "py_generic_services" (py_generic_services msg)
       P'.tellT "deprecated" (deprecated msg)
       P'.tellT "cc_enable_arenas" (cc_enable_arenas msg)
       P'.tellT "objc_class_prefix" (objc_class_prefix msg)
       P'.tellT "csharp_namespace" (csharp_namespace msg)
       P'.tellT "javanano_use_deprecated_package" (javanano_use_deprecated_package msg)
       P'.tellT "uninterpreted_option" (uninterpreted_option msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'java_package, parse'java_outer_classname, parse'java_multiple_files, parse'java_generate_equals_and_hash,
                   parse'java_string_check_utf8, parse'optimize_for, parse'go_package, parse'cc_generic_services,
                   parse'java_generic_services, parse'py_generic_services, parse'deprecated, parse'cc_enable_arenas,
                   parse'objc_class_prefix, parse'csharp_namespace, parse'javanano_use_deprecated_package,
                   parse'uninterpreted_option])
                P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'java_package
         = P'.try
            (do
               v <- P'.getT "java_package"
               Prelude'.return (\ o -> o{java_package = v}))
        parse'java_outer_classname
         = P'.try
            (do
               v <- P'.getT "java_outer_classname"
               Prelude'.return (\ o -> o{java_outer_classname = v}))
        parse'java_multiple_files
         = P'.try
            (do
               v <- P'.getT "java_multiple_files"
               Prelude'.return (\ o -> o{java_multiple_files = v}))
        parse'java_generate_equals_and_hash
         = P'.try
            (do
               v <- P'.getT "java_generate_equals_and_hash"
               Prelude'.return (\ o -> o{java_generate_equals_and_hash = v}))
        parse'java_string_check_utf8
         = P'.try
            (do
               v <- P'.getT "java_string_check_utf8"
               Prelude'.return (\ o -> o{java_string_check_utf8 = v}))
        parse'optimize_for
         = P'.try
            (do
               v <- P'.getT "optimize_for"
               Prelude'.return (\ o -> o{optimize_for = v}))
        parse'go_package
         = P'.try
            (do
               v <- P'.getT "go_package"
               Prelude'.return (\ o -> o{go_package = v}))
        parse'cc_generic_services
         = P'.try
            (do
               v <- P'.getT "cc_generic_services"
               Prelude'.return (\ o -> o{cc_generic_services = v}))
        parse'java_generic_services
         = P'.try
            (do
               v <- P'.getT "java_generic_services"
               Prelude'.return (\ o -> o{java_generic_services = v}))
        parse'py_generic_services
         = P'.try
            (do
               v <- P'.getT "py_generic_services"
               Prelude'.return (\ o -> o{py_generic_services = v}))
        parse'deprecated
         = P'.try
            (do
               v <- P'.getT "deprecated"
               Prelude'.return (\ o -> o{deprecated = v}))
        parse'cc_enable_arenas
         = P'.try
            (do
               v <- P'.getT "cc_enable_arenas"
               Prelude'.return (\ o -> o{cc_enable_arenas = v}))
        parse'objc_class_prefix
         = P'.try
            (do
               v <- P'.getT "objc_class_prefix"
               Prelude'.return (\ o -> o{objc_class_prefix = v}))
        parse'csharp_namespace
         = P'.try
            (do
               v <- P'.getT "csharp_namespace"
               Prelude'.return (\ o -> o{csharp_namespace = v}))
        parse'javanano_use_deprecated_package
         = P'.try
            (do
               v <- P'.getT "javanano_use_deprecated_package"
               Prelude'.return (\ o -> o{javanano_use_deprecated_package = v}))
        parse'uninterpreted_option
         = P'.try
            (do
               v <- P'.getT "uninterpreted_option"
               Prelude'.return (\ o -> o{uninterpreted_option = P'.append (uninterpreted_option o) v}))