{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Text.DescriptorProtos.DescriptorProto (DescriptorProto(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as DescriptorProtos.DescriptorProto (ExtensionRange)
import qualified Text.DescriptorProtos.DescriptorProto.ReservedRange as DescriptorProtos.DescriptorProto (ReservedRange)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos (EnumDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos (FieldDescriptorProto)
import qualified Text.DescriptorProtos.MessageOptions as DescriptorProtos (MessageOptions)
import qualified Text.DescriptorProtos.OneofDescriptorProto as DescriptorProtos (OneofDescriptorProto)
 
data DescriptorProto = DescriptorProto{name :: !(P'.Maybe P'.Utf8), field :: !(P'.Seq DescriptorProtos.FieldDescriptorProto),
                                       extension :: !(P'.Seq DescriptorProtos.FieldDescriptorProto),
                                       nested_type :: !(P'.Seq DescriptorProto),
                                       enum_type :: !(P'.Seq DescriptorProtos.EnumDescriptorProto),
                                       extension_range :: !(P'.Seq DescriptorProtos.DescriptorProto.ExtensionRange),
                                       oneof_decl :: !(P'.Seq DescriptorProtos.OneofDescriptorProto),
                                       options :: !(P'.Maybe DescriptorProtos.MessageOptions),
                                       reserved_range :: !(P'.Seq DescriptorProtos.DescriptorProto.ReservedRange),
                                       reserved_name :: !(P'.Seq P'.Utf8), unknown'field :: !(P'.UnknownField)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage DescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable DescriptorProto where
  mergeAppend (DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   (DescriptorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11)
   = DescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
 
instance P'.Default DescriptorProto where
  defaultValue
   = DescriptorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire DescriptorProto where
  wireSize ft' self'@(DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeRep 1 11 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeRep 1 11 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeRep 1 11 x'9
             + P'.wireSizeRep 1 9 x'10
             + P'.wireSizeUnknownField x'11)
  wirePut ft' self'@(DescriptorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11)
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
             P'.wirePutRep 26 11 x'4
             P'.wirePutRep 34 11 x'5
             P'.wirePutRep 42 11 x'6
             P'.wirePutRep 50 11 x'3
             P'.wirePutOpt 58 11 x'8
             P'.wirePutRep 66 11 x'7
             P'.wirePutRep 74 11 x'9
             P'.wirePutRep 82 9 x'10
             P'.wirePutUnknownField x'11
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{field = P'.append (field old'Self) new'Field}) (P'.wireGet 11)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{extension = P'.append (extension old'Self) new'Field}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{nested_type = P'.append (nested_type old'Self) new'Field})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{enum_type = P'.append (enum_type old'Self) new'Field}) (P'.wireGet 11)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{extension_range = P'.append (extension_range old'Self) new'Field})
                    (P'.wireGet 11)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{oneof_decl = P'.append (oneof_decl old'Self) new'Field}) (P'.wireGet 11)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{options = P'.mergeAppend (options old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{reserved_range = P'.append (reserved_range old'Self) new'Field})
                    (P'.wireGet 11)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{reserved_name = P'.append (reserved_name old'Self) new'Field})
                    (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> DescriptorProto) DescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB DescriptorProto
 
instance P'.ReflectDescriptor DescriptorProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42, 50, 58, 66, 74, 82])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"DescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.field\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"field\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.extension\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"extension\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.nested_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"nested_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"DescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.enum_type\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"enum_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.EnumDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"EnumDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.extension_range\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"extension_range\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto.ExtensionRange\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName = MName \"ExtensionRange\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.oneof_decl\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"oneof_decl\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.OneofDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"OneofDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.options\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"options\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.MessageOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"MessageOptions\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.reserved_range\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"reserved_range\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto.ReservedRange\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName = MName \"ReservedRange\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.reserved_name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName' = FName \"reserved_name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"
 
instance P'.TextType DescriptorProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg DescriptorProto where
  textPut msg
   = do
       P'.tellT "name" (name msg)
       P'.tellT "field" (field msg)
       P'.tellT "extension" (extension msg)
       P'.tellT "nested_type" (nested_type msg)
       P'.tellT "enum_type" (enum_type msg)
       P'.tellT "extension_range" (extension_range msg)
       P'.tellT "oneof_decl" (oneof_decl msg)
       P'.tellT "options" (options msg)
       P'.tellT "reserved_range" (reserved_range msg)
       P'.tellT "reserved_name" (reserved_name msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'name, parse'field, parse'extension, parse'nested_type, parse'enum_type, parse'extension_range,
                   parse'oneof_decl, parse'options, parse'reserved_range, parse'reserved_name])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'field
         = P'.try
            (do
               v <- P'.getT "field"
               Prelude'.return (\ o -> o{field = P'.append (field o) v}))
        parse'extension
         = P'.try
            (do
               v <- P'.getT "extension"
               Prelude'.return (\ o -> o{extension = P'.append (extension o) v}))
        parse'nested_type
         = P'.try
            (do
               v <- P'.getT "nested_type"
               Prelude'.return (\ o -> o{nested_type = P'.append (nested_type o) v}))
        parse'enum_type
         = P'.try
            (do
               v <- P'.getT "enum_type"
               Prelude'.return (\ o -> o{enum_type = P'.append (enum_type o) v}))
        parse'extension_range
         = P'.try
            (do
               v <- P'.getT "extension_range"
               Prelude'.return (\ o -> o{extension_range = P'.append (extension_range o) v}))
        parse'oneof_decl
         = P'.try
            (do
               v <- P'.getT "oneof_decl"
               Prelude'.return (\ o -> o{oneof_decl = P'.append (oneof_decl o) v}))
        parse'options
         = P'.try
            (do
               v <- P'.getT "options"
               Prelude'.return (\ o -> o{options = v}))
        parse'reserved_range
         = P'.try
            (do
               v <- P'.getT "reserved_range"
               Prelude'.return (\ o -> o{reserved_range = P'.append (reserved_range o) v}))
        parse'reserved_name
         = P'.try
            (do
               v <- P'.getT "reserved_name"
               Prelude'.return (\ o -> o{reserved_name = P'.append (reserved_name o) v}))