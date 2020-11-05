{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Text.DescriptorProtos.SourceCodeInfo.Location (Location(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.List as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Location = Location{path :: !(P'.Seq P'.Int32), span :: !(P'.Seq P'.Int32), leading_comments :: !(P'.Maybe P'.Utf8),
                         trailing_comments :: !(P'.Maybe P'.Utf8), leading_detached_comments :: !(P'.Seq P'.Utf8),
                         unknown'field :: !(P'.UnknownField)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage Location where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable Location where
  mergeAppend (Location x'1 x'2 x'3 x'4 x'5 x'6) (Location y'1 y'2 y'3 y'4 y'5 y'6)
   = let !z'1 = P'.mergeAppend x'1 y'1
         !z'2 = P'.mergeAppend x'2 y'2
         !z'3 = P'.mergeAppend x'3 y'3
         !z'4 = P'.mergeAppend x'4 y'4
         !z'5 = P'.mergeAppend x'5 y'5
         !z'6 = P'.mergeAppend x'6 y'6
      in Location z'1 z'2 z'3 z'4 z'5 z'6

instance P'.Default Location where
  defaultValue = Location P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire Location where
  wireSize ft' self'@(Location x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizePacked 1 5 x'1 + P'.wireSizePacked 1 5 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 9 x'4 +
             P'.wireSizeRep 1 9 x'5
             + P'.wireSizeUnknownField x'6)
  wirePutWithSize ft' self'@(Location x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutPackedWithSize 10 5 x'1, P'.wirePutPackedWithSize 18 5 x'2, P'.wirePutOptWithSize 26 9 x'3,
             P'.wirePutOptWithSize 34 9 x'4, P'.wirePutRepWithSize 50 9 x'5, P'.wirePutUnknownFieldWithSize x'6]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{path = P'.append (path old'Self) new'Field}) (P'.wireGet 5)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{path = P'.mergeAppend (path old'Self) new'Field}) (P'.wireGetPacked 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{span = P'.append (span old'Self) new'Field}) (P'.wireGet 5)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{span = P'.mergeAppend (span old'Self) new'Field}) (P'.wireGetPacked 5)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{leading_comments = Prelude'.Just new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{trailing_comments = Prelude'.Just new'Field}) (P'.wireGet 9)
             50 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{leading_detached_comments = P'.append (leading_detached_comments old'Self) new'Field})
                    (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Location) Location where
  getVal m' f' = f' m'

instance P'.GPB Location

instance P'.ReflectDescriptor Location where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 10, 16, 18, 26, 34, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.SourceCodeInfo.Location\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\"], baseName = MName \"Location\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"SourceCodeInfo\",\"Location.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.path\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"path\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.span\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"span\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.leading_comments\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"leading_comments\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.trailing_comments\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"trailing_comments\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.leading_detached_comments\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"leading_detached_comments\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Location where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Location where
  textPut msg
   = do
       P'.tellT "path" (path msg)
       P'.tellT "span" (span msg)
       P'.tellT "leading_comments" (leading_comments msg)
       P'.tellT "trailing_comments" (trailing_comments msg)
       P'.tellT "leading_detached_comments" (leading_detached_comments msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'path, parse'span, parse'leading_comments, parse'trailing_comments, parse'leading_detached_comments])
                P'.spaces
       Prelude'.return (Prelude'.foldl' (\ v f -> f v) P'.defaultValue mods)
    where
        parse'path
         = P'.try
            (do
               v <- P'.getT "path"
               Prelude'.return (\ o -> o{path = P'.append (path o) v}))
        parse'span
         = P'.try
            (do
               v <- P'.getT "span"
               Prelude'.return (\ o -> o{span = P'.append (span o) v}))
        parse'leading_comments
         = P'.try
            (do
               v <- P'.getT "leading_comments"
               Prelude'.return (\ o -> o{leading_comments = v}))
        parse'trailing_comments
         = P'.try
            (do
               v <- P'.getT "trailing_comments"
               Prelude'.return (\ o -> o{trailing_comments = v}))
        parse'leading_detached_comments
         = P'.try
            (do
               v <- P'.getT "leading_detached_comments"
               Prelude'.return (\ o -> o{leading_detached_comments = P'.append (leading_detached_comments o) v}))