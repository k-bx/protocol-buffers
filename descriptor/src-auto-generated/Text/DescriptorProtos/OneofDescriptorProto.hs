{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Text.DescriptorProtos.OneofDescriptorProto (OneofDescriptorProto(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data OneofDescriptorProto = OneofDescriptorProto{name :: !(P'.Maybe P'.Utf8), unknown'field :: !(P'.UnknownField)}
                          deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage OneofDescriptorProto where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable OneofDescriptorProto where
  mergeAppend (OneofDescriptorProto x'1 x'2) (OneofDescriptorProto y'1 y'2)
   = OneofDescriptorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OneofDescriptorProto where
  defaultValue = OneofDescriptorProto P'.defaultValue P'.defaultValue
 
instance P'.Wire OneofDescriptorProto where
  wireSize ft' self'@(OneofDescriptorProto x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeUnknownField x'2)
  wirePut ft' self'@(OneofDescriptorProto x'1 x'2)
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
             P'.wirePutUnknownField x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OneofDescriptorProto) OneofDescriptorProto where
  getVal m' f' = f' m'
 
instance P'.GPB OneofDescriptorProto
 
instance P'.ReflectDescriptor OneofDescriptorProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.OneofDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"OneofDescriptorProto\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"OneofDescriptorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.OneofDescriptorProto.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"OneofDescriptorProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"
 
instance P'.TextType OneofDescriptorProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg OneofDescriptorProto where
  textPut msg
   = do
       P'.tellT "name" (name msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'name]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))