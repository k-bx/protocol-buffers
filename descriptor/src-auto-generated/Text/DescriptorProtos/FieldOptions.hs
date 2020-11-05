{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.FieldOptions (FieldOptions(..)) where
import Prelude ((+), (/), (++), (.), (==), (<=), (&&))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions (CType)
import qualified Text.DescriptorProtos.FieldOptions.JSType as DescriptorProtos.FieldOptions (JSType)
import qualified Text.DescriptorProtos.UninterpretedOption as DescriptorProtos (UninterpretedOption)

data FieldOptions = FieldOptions{ctype :: !(P'.Maybe DescriptorProtos.FieldOptions.CType), packed :: !(P'.Maybe P'.Bool),
                                 jstype :: !(P'.Maybe DescriptorProtos.FieldOptions.JSType), lazy :: !(P'.Maybe P'.Bool),
                                 deprecated :: !(P'.Maybe P'.Bool), weak :: !(P'.Maybe P'.Bool),
                                 uninterpreted_option :: !(P'.Seq DescriptorProtos.UninterpretedOption),
                                 ext'field :: !(P'.ExtField), unknown'field :: !(P'.UnknownField)}
                    deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.ExtendMessage FieldOptions where
  getExtField = ext'field
  putExtField e'f msg = msg{ext'field = e'f}
  validExtRanges msg = P'.extRanges (P'.reflectDescriptorInfo msg)

instance P'.UnknownMessage FieldOptions where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable FieldOptions where
  mergeAppend (FieldOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (FieldOptions y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
         !z'7 = P'.mergeAppend x'7 y'7
         !z'8 = P'.mergeAppend x'8 y'8
         !z'9 = P'.mergeAppend x'9 y'9
      in FieldOptions z'1 z'2 z'3 z'4 z'5 z'6 z'7 z'8 z'9

instance P'.Default FieldOptions where
  defaultValue
   = FieldOptions (Prelude'.Just (Prelude'.read "STRING")) P'.defaultValue (Prelude'.Just (Prelude'.read "JS_NORMAL"))
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      (Prelude'.Just Prelude'.False)
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire FieldOptions where
  wireSize ft' self'@(FieldOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 8 x'2 + P'.wireSizeOpt 1 14 x'3 + P'.wireSizeOpt 1 8 x'4 +
             P'.wireSizeOpt 1 8 x'5
             + P'.wireSizeOpt 1 8 x'6
             + P'.wireSizeRep 2 11 x'7
             + P'.wireSizeExtField x'8
             + P'.wireSizeUnknownField x'9)
  wirePutWithSize ft' self'@(FieldOptions x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 14 x'1, P'.wirePutOptWithSize 16 8 x'2, P'.wirePutOptWithSize 24 8 x'5,
             P'.wirePutOptWithSize 40 8 x'4, P'.wirePutOptWithSize 48 14 x'3, P'.wirePutOptWithSize 80 8 x'6,
             P'.wirePutRepWithSize 7994 11 x'7, P'.wirePutExtFieldWithSize x'8, P'.wirePutUnknownFieldWithSize x'9]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{ctype = Prelude'.Just new'Field}) (P'.wireGet 14)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{packed = Prelude'.Just new'Field}) (P'.wireGet 8)
             48 -> Prelude'.fmap (\ !new'Field -> old'Self{jstype = Prelude'.Just new'Field}) (P'.wireGet 14)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{lazy = Prelude'.Just new'Field}) (P'.wireGet 8)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{deprecated = Prelude'.Just new'Field}) (P'.wireGet 8)
             80 -> Prelude'.fmap (\ !new'Field -> old'Self{weak = Prelude'.Just new'Field}) (P'.wireGet 8)
             7994 -> Prelude'.fmap
                      (\ !new'Field -> old'Self{uninterpreted_option = P'.append (uninterpreted_option old'Self) new'Field})
                      (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in
                   if Prelude'.or [1000 <= field'Number && field'Number <= 18999, 20000 <= field'Number] then
                    P'.loadExtension field'Number wire'Type old'Self else P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> FieldOptions) FieldOptions where
  getVal m' f' = f' m'

instance P'.GPB FieldOptions

instance P'.ReflectDescriptor FieldOptions where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16, 24, 40, 48, 80, 7994])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FieldOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FieldOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FieldOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.ctype\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"ctype\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldOptions.CType\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName = MName \"CType\"}), hsRawDefault = Just \"STRING\", hsDefault = Just (HsDef'Enum \"STRING\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.packed\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"packed\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.jstype\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"jstype\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 48}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FieldOptions.JSType\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName = MName \"JSType\"}), hsRawDefault = Just \"JS_NORMAL\", hsDefault = Just (HsDef'Enum \"JS_NORMAL\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.lazy\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"lazy\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.deprecated\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"deprecated\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.weak\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"weak\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FieldOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FieldOptions\"], baseName' = FName \"uninterpreted_option\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType FieldOptions where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg FieldOptions where
  textPut msg
   = do
       P'.tellT "ctype" (ctype msg)
       P'.tellT "packed" (packed msg)
       P'.tellT "jstype" (jstype msg)
       P'.tellT "lazy" (lazy msg)
       P'.tellT "deprecated" (deprecated msg)
       P'.tellT "weak" (weak msg)
       P'.tellT "uninterpreted_option" (uninterpreted_option msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'ctype, parse'packed, parse'jstype, parse'lazy, parse'deprecated, parse'weak, parse'uninterpreted_option])
                P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'ctype = Prelude'.fmap (\ v o -> o{ctype = v}) (P'.try (P'.getT "ctype"))
        parse'packed = Prelude'.fmap (\ v o -> o{packed = v}) (P'.try (P'.getT "packed"))
        parse'jstype = Prelude'.fmap (\ v o -> o{jstype = v}) (P'.try (P'.getT "jstype"))
        parse'lazy = Prelude'.fmap (\ v o -> o{lazy = v}) (P'.try (P'.getT "lazy"))
        parse'deprecated = Prelude'.fmap (\ v o -> o{deprecated = v}) (P'.try (P'.getT "deprecated"))
        parse'weak = Prelude'.fmap (\ v o -> o{weak = v}) (P'.try (P'.getT "weak"))
        parse'uninterpreted_option
         = Prelude'.fmap (\ v o -> o{uninterpreted_option = P'.append (uninterpreted_option o) v})
            (P'.try (P'.getT "uninterpreted_option"))