{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.UninterpretedOption (UninterpretedOption(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart as DescriptorProtos.UninterpretedOption (NamePart)

data UninterpretedOption = UninterpretedOption{name :: !(P'.Seq DescriptorProtos.UninterpretedOption.NamePart),
                                               identifier_value :: !(P'.Maybe P'.Utf8), positive_int_value :: !(P'.Maybe P'.Word64),
                                               negative_int_value :: !(P'.Maybe P'.Int64), double_value :: !(P'.Maybe P'.Double),
                                               string_value :: !(P'.Maybe P'.ByteString), aggregate_value :: !(P'.Maybe P'.Utf8),
                                               unknown'field :: !(P'.UnknownField)}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage UninterpretedOption where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable UninterpretedOption where
  mergeAppend (UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8) (UninterpretedOption y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8)
   = UninterpretedOption (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)

instance P'.Default UninterpretedOption where
  defaultValue
   = UninterpretedOption P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire UninterpretedOption where
  wireSize ft' self'@(UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 4 x'3 + P'.wireSizeOpt 1 3 x'4 +
             P'.wireSizeOpt 1 1 x'5
             + P'.wireSizeOpt 1 12 x'6
             + P'.wireSizeOpt 1 9 x'7
             + P'.wireSizeUnknownField x'8)
  wirePutWithSize ft' self'@(UninterpretedOption x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 18 11 x'1, P'.wirePutOptWithSize 26 9 x'2, P'.wirePutOptWithSize 32 4 x'3,
             P'.wirePutOptWithSize 40 3 x'4, P'.wirePutOptWithSize 49 1 x'5, P'.wirePutOptWithSize 58 12 x'6,
             P'.wirePutOptWithSize 66 9 x'7, P'.wirePutUnknownFieldWithSize x'8]
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
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{name = P'.append (name old'Self) new'Field}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{identifier_value = Prelude'.Just new'Field}) (P'.wireGet 9)
             32 -> Prelude'.fmap (\ !new'Field -> old'Self{positive_int_value = Prelude'.Just new'Field}) (P'.wireGet 4)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{negative_int_value = Prelude'.Just new'Field}) (P'.wireGet 3)
             49 -> Prelude'.fmap (\ !new'Field -> old'Self{double_value = Prelude'.Just new'Field}) (P'.wireGet 1)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{string_value = Prelude'.Just new'Field}) (P'.wireGet 12)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{aggregate_value = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> UninterpretedOption) UninterpretedOption where
  getVal m' f' = f' m'

instance P'.GPB UninterpretedOption

instance P'.ReflectDescriptor UninterpretedOption where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [18, 26, 32, 40, 49, 58, 66])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"UninterpretedOption.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption.NamePart\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName = MName \"NamePart\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.identifier_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"identifier_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.positive_int_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"positive_int_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.negative_int_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"negative_int_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.double_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"double_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.string_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"string_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.aggregate_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"aggregate_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType UninterpretedOption where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg UninterpretedOption where
  textPut msg
   = do
       P'.tellT "name" (name msg)
       P'.tellT "identifier_value" (identifier_value msg)
       P'.tellT "positive_int_value" (positive_int_value msg)
       P'.tellT "negative_int_value" (negative_int_value msg)
       P'.tellT "double_value" (double_value msg)
       P'.tellT "string_value" (string_value msg)
       P'.tellT "aggregate_value" (aggregate_value msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'name, parse'identifier_value, parse'positive_int_value, parse'negative_int_value, parse'double_value,
                   parse'string_value, parse'aggregate_value])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = P'.append (name o) v}))
        parse'identifier_value
         = P'.try
            (do
               v <- P'.getT "identifier_value"
               Prelude'.return (\ o -> o{identifier_value = v}))
        parse'positive_int_value
         = P'.try
            (do
               v <- P'.getT "positive_int_value"
               Prelude'.return (\ o -> o{positive_int_value = v}))
        parse'negative_int_value
         = P'.try
            (do
               v <- P'.getT "negative_int_value"
               Prelude'.return (\ o -> o{negative_int_value = v}))
        parse'double_value
         = P'.try
            (do
               v <- P'.getT "double_value"
               Prelude'.return (\ o -> o{double_value = v}))
        parse'string_value
         = P'.try
            (do
               v <- P'.getT "string_value"
               Prelude'.return (\ o -> o{string_value = v}))
        parse'aggregate_value
         = P'.try
            (do
               v <- P'.getT "aggregate_value"
               Prelude'.return (\ o -> o{aggregate_value = v}))