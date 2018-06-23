{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Text.DescriptorProtos.DescriptorProto.ExtensionRange (ExtensionRange(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data ExtensionRange = ExtensionRange{start :: !(P'.Maybe P'.Int32), end :: !(P'.Maybe P'.Int32),
                                     unknown'field :: !(P'.UnknownField)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage ExtensionRange where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable ExtensionRange where
  mergeAppend (ExtensionRange x'1 x'2 x'3) (ExtensionRange y'1 y'2 y'3)
   = ExtensionRange (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default ExtensionRange where
  defaultValue = ExtensionRange P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire ExtensionRange where
  wireSize ft' self'@(ExtensionRange x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeUnknownField x'3)
  wirePutWithSize ft' self'@(ExtensionRange x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 5 x'1, P'.wirePutOptWithSize 16 5 x'2, P'.wirePutUnknownFieldWithSize x'3]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{start = Prelude'.Just new'Field}) (P'.wireGet 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{end = Prelude'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ExtensionRange) ExtensionRange where
  getVal m' f' = f' m'

instance P'.GPB ExtensionRange

instance P'.ReflectDescriptor ExtensionRange where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.DescriptorProto.ExtensionRange\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"DescriptorProto\"], baseName = MName \"ExtensionRange\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"DescriptorProto\",\"ExtensionRange.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.ExtensionRange.start\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\",MName \"ExtensionRange\"], baseName' = FName \"start\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.DescriptorProto.ExtensionRange.end\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"DescriptorProto\",MName \"ExtensionRange\"], baseName' = FName \"end\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"

instance P'.TextType ExtensionRange where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ExtensionRange where
  textPut msg
   = do
       P'.tellT "start" (start msg)
       P'.tellT "end" (end msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'start, parse'end]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'start
         = P'.try
            (do
               v <- P'.getT "start"
               Prelude'.return (\ o -> o{start = v}))
        parse'end
         = P'.try
            (do
               v <- P'.getT "end"
               Prelude'.return (\ o -> o{end = v}))