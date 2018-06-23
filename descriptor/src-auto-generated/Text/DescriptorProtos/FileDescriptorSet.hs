{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Text.DescriptorProtos.FileDescriptorSet (FileDescriptorSet(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Text.DescriptorProtos.FileDescriptorProto as DescriptorProtos (FileDescriptorProto)

data FileDescriptorSet = FileDescriptorSet{file :: !(P'.Seq DescriptorProtos.FileDescriptorProto),
                                           unknown'field :: !(P'.UnknownField)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.UnknownMessage FileDescriptorSet where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}

instance P'.Mergeable FileDescriptorSet where
  mergeAppend (FileDescriptorSet x'1 x'2) (FileDescriptorSet y'1 y'2)
   = FileDescriptorSet (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default FileDescriptorSet where
  defaultValue = FileDescriptorSet P'.defaultValue P'.defaultValue

instance P'.Wire FileDescriptorSet where
  wireSize ft' self'@(FileDescriptorSet x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeUnknownField x'2)
  wirePutWithSize ft' self'@(FileDescriptorSet x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutRepWithSize 10 11 x'1, P'.wirePutUnknownFieldWithSize x'2]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{file = P'.append (file old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> FileDescriptorSet) FileDescriptorSet where
  getVal m' f' = f' m'

instance P'.GPB FileDescriptorSet

instance P'.ReflectDescriptor FileDescriptorSet where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileDescriptorSet\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileDescriptorSet\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileDescriptorSet.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileDescriptorSet.file\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileDescriptorSet\"], baseName' = FName \"file\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileDescriptorProto\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileDescriptorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"

instance P'.TextType FileDescriptorSet where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg FileDescriptorSet where
  textPut msg
   = do
       P'.tellT "file" (file msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'file]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'file
         = P'.try
            (do
               v <- P'.getT "file"
               Prelude'.return (\ o -> o{file = P'.append (file o) v}))