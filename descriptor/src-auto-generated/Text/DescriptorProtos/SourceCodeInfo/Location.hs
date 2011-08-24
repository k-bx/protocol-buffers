{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Text.DescriptorProtos.SourceCodeInfo.Location (Location(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Location = Location{path :: !(P'.Seq P'.Int32), span :: !(P'.Seq P'.Int32), unknown'field :: !P'.UnknownField}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.UnknownMessage Location where
  getUnknownField = unknown'field
  putUnknownField u'f msg = msg{unknown'field = u'f}
 
instance P'.Mergeable Location where
  mergeAppend (Location x'1 x'2 x'3) (Location y'1 y'2 y'3)
   = Location (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default Location where
  defaultValue = Location P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire Location where
  wireSize ft' self'@(Location x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizePacked 1 5 x'1 + P'.wireSizePacked 1 5 x'2 + P'.wireSizeUnknownField x'3)
  wirePut ft' self'@(Location x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutPacked 10 5 x'1
             P'.wirePutPacked 18 5 x'2
             P'.wirePutUnknownField x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{path = P'.append (path old'Self) new'Field}) (P'.wireGet 5)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{path = P'.mergeAppend (path old'Self) new'Field}) (P'.wireGetPacked 5)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{span = P'.append (span old'Self) new'Field}) (P'.wireGet 5)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{span = P'.mergeAppend (span old'Self) new'Field}) (P'.wireGetPacked 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Location) Location where
  getVal m' f' = f' m'
 
instance P'.GPB Location
 
instance P'.ReflectDescriptor Location where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 10, 16, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.SourceCodeInfo.Location\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\"], baseName = MName \"Location\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"SourceCodeInfo\",\"Location.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.path\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"path\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.SourceCodeInfo.Location.span\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"SourceCodeInfo\",MName \"Location\"], baseName' = FName \"span\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Just (WireTag {getWireTag = 16},WireTag {getWireTag = 18}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False}"