{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.DescriptorProtos.FileOptions (FileOptions(..)) where
import Prelude ((+), (/), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions (OptimizeMode)
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)
 
data FileOptions = FileOptions{java_package :: !(P'.Maybe P'.Utf8), java_outer_classname :: !(P'.Maybe P'.Utf8),
                               java_multiple_files :: !(P'.Maybe P'.Bool), java_generate_equals_and_hash :: !(P'.Maybe P'.Bool),
                               optimize_for :: !(P'.Maybe DescriptorProtos.FileOptions.OptimizeMode),
                               cc_generic_services :: !(P'.Maybe P'.Bool), java_generic_services :: !(P'.Maybe P'.Bool),
                               py_generic_services :: !(P'.Maybe P'.Bool),
                               uninterpreted_option :: !(P'.Seq DescriptorProtos.UninterpretedOption), ext'field :: !P'.ExtField,
                               unknown'field :: !P'.UnknownField}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.ExtendMessage FileOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)
 
instance P'.UnknownMessage FileOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable FileOptions where
  mergeAppend (FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   (FileOptions y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = FileOptions (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
 
instance P'.Default FileOptions where
  defaultValue
   = FileOptions P'.defaultValue P'.defaultValue (Prelude'.Just Prelude'.False) (Prelude'.Just Prelude'.False)
      (Prelude'.Just (Prelude'.read "SPEED"))
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire FileOptions where
  wireSize ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 8 x'3 + P'.wireSizeOpt 2 8 x'4 +
             P'.wireSizeOpt 1 14 x'5
             + P'.wireSizeOpt 2 8 x'6
             + P'.wireSizeOpt 2 8 x'7
             + P'.wireSizeOpt 2 8 x'8
             + P'.wireSizeRep 2 11 x'9
             + P'.wireSizeExtField x'10
             + P'.wireSizeUnknownField x'11)
  wirePut ft' self'@(FileOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
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
             P'.wirePutOpt 66 9 x'2
             P'.wirePutOpt 72 14 x'5
             P'.wirePutOpt 80 8 x'3
             P'.wirePutOpt 128 8 x'6
             P'.wirePutOpt 136 8 x'7
             P'.wirePutOpt 144 8 x'8
             P'.wirePutOpt 160 8 x'4
             P'.wirePutRep 7994 11 x'9
             P'.wirePutExtField x'10
             P'.wirePutUnknownField x'11
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{java_package = Prelude'.Just new'Field}) (P'.wireGet 9)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{java_outer_classname = Prelude'.Just new'Field}) (P'.wireGet 9)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{java_multiple_files = Prelude'.Just new'Field}) (P'.wireGet 8)
             160 -> Prelude'.fmap (\ !new'Field -> old'Self{java_generate_equals_and_hash = Prelude'.Just new'Field}) (P'.wireGet 8)
             72 -> Prelude'.fmap (\ !new'Field -> old'Self{optimize_for = Prelude'.Just new'Field}) (P'.wireGet 14)
             128 -> Prelude'.fmap (\ !new'Field -> old'Self{cc_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
             136 -> Prelude'.fmap (\ !new'Field -> old'Self{java_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
             144 -> Prelude'.fmap (\ !new'Field -> old'Self{py_generic_services = Prelude'.Just new'Field}) (P'.wireGet 8)
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
   = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 66, 72, 80, 128, 136, 144, 160, 7994])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_package\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_outer_classname\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_outer_classname\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_multiple_files\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_multiple_files\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generate_equals_and_hash\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generate_equals_and_hash\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.optimize_for\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"optimize_for\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileOptions.OptimizeMode\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName = MName \"OptimizeMode\"}), hsRawDefault = Just (Chunk \"SPEED\" Empty), hsDefault = Just (HsDef'Enum \"SPEED\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.cc_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"cc_generic_services\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generic_services\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.py_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"py_generic_services\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just (Chunk \"false\" Empty), hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"uninterpreted_option\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"