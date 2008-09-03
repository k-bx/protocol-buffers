module Text.ProtocolBuffers.MakeReflections(makeProtoInfo,makeEnumInfo,makeDescriptorInfo) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..)) 
import qualified Text.DescriptorProtos.EnumOptions                    as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions                    as D.EnumOptions(EnumOptions(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueOptions               as D(EnumValueOptions) 
import qualified Text.DescriptorProtos.EnumValueOptions               as D.EnumValueOptions(EnumValueOptions(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import           Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto(FileDescriptorProto)) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.WireMessage(size'Varint,toWireTag)

import qualified Data.Foldable as F(foldr,toList)
import qualified Data.ByteString as S(concat)
import qualified Data.ByteString.Char8 as SC(spanEnd)
import qualified Data.ByteString.Lazy.Char8 as LC(toChunks,fromChunks,length,init,unpack)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString,toString)
import Data.List(partition,unfoldr)
--import Data.Sequence(viewl,ViewL(..),(|>))
import qualified Data.Sequence as Seq(fromList,empty,singleton)
import Numeric(readHex,readOct,readDec)
import Data.Map(Map)
import Data.Monoid(mconcat,mappend)
import qualified Data.Map as M(empty,fromListWith,lookup)
import Data.Maybe(fromMaybe,catMaybes)
import System.FilePath

spanEndL f bs = let (a,b) = SC.spanEnd f (S.concat . LC.toChunks $ bs)
                in (LC.fromChunks [a],LC.fromChunks [b])

-- Take a bytestring of "A" into "Right A" and "A.B.C" into "Left (A.B,C)"
splitMod :: Utf8 -> Either (Utf8,Utf8) Utf8
splitMod (Utf8 bs) = case spanEndL ('.'/=) bs of
                       (pre,post) | LC.length pre <= 1 -> Right (Utf8 bs)
                                  | otherwise -> Left (Utf8 (LC.init pre),Utf8 post)

toString = U.toString . utf8

toProtoName :: String -> Utf8 -> ProtoName
toProtoName prefix rawName =
  case splitMod rawName of
    Left (m,b) -> ProtoName prefix (toString m) (toString b)
    Right b    -> ProtoName prefix ""           (toString b)

toPath :: String -> Utf8 -> [FilePath]
toPath prefix name = splitDirectories (combine a b)
  where a = joinPath . splitDot $ prefix
        b = flip addExtension "hs" . joinPath . splitDot . U.toString . utf8 $ name
        splitDot :: String -> [FilePath]
        splitDot = unfoldr s where
            s ('.':xs) = s xs
            s [] = Nothing
            s xs = Just (span ('.'/=) xs)

dotPre :: String -> String -> String
dotPre "" x = x
dotPre x "" = x
dotPre s x@('.':xs)  | '.' == last s = s ++ xs
                     | otherwise = s ++ x
dotPre s x | '.' == last s = s++x
           | otherwise = s++('.':x)


makeProtoInfo :: String -> [String] -> D.FileDescriptorProto -> ProtoInfo
makeProtoInfo prefix names
              fdp@(D.FileDescriptorProto
                    { D.FileDescriptorProto.name = Just rawName })
     = ProtoInfo protoName (toPath prefix rawName) keyInfos allMessages allEnums allKeys where
  protoName = case names of
                [] -> ProtoName prefix "" ""
                [name] -> ProtoName prefix "" name
                _ -> ProtoName prefix (foldr1 (\a bs -> a  ++ ('.' : bs)) (init names)) (last names)
  keyInfos = Seq.fromList . map (\f -> (keyExtendee prefix f,toFieldInfo protoName f))
             . F.toList . D.FileDescriptorProto.extension $ fdp
  allMessages = concatMap (processMSG False) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allEnums = map (makeEnumInfo prefix) (F.toList $ D.FileDescriptorProto.enum_type fdp) 
             ++ concatMap processENM (F.toList $ D.FileDescriptorProto.message_type fdp)
  allKeys = M.fromListWith mappend . map (\(k,a) -> (k,Seq.singleton a))
            . F.toList . mconcat $ keyInfos : map keys allMessages

  processMSG isGroup msg = 
    let getKnownKeys protoName = fromMaybe Seq.empty (M.lookup protoName allKeys)
        groups = collectedGroups msg
        checkGroup x = elem (fromMaybe (error $ "no message name:\n"++show msg)
                                       (D.DescriptorProto.name x))
                            groups
    in makeDescriptorInfo getKnownKeys prefix isGroup msg
       : concatMap (\x -> processMSG (checkGroup x) x)
                   (F.toList (D.DescriptorProto.nested_type msg))
  processENM msg = foldr ((:) . makeEnumInfo prefix) nested
                         (F.toList (D.DescriptorProto.enum_type msg))
    where nested = concatMap processENM (F.toList (D.DescriptorProto.nested_type msg))

collectedGroups :: D.DescriptorProto -> [Utf8] 
collectedGroups = catMaybes
                . map D.FieldDescriptorProto.type_name
                . filter (\f -> D.FieldDescriptorProto.type' f == Just TYPE_GROUP) 
                . F.toList
                . D.DescriptorProto.field

makeEnumInfo :: String -> D.EnumDescriptorProto -> EnumInfo
makeEnumInfo prefix e@(D.EnumDescriptorProto.EnumDescriptorProto
                        { D.EnumDescriptorProto.name = Just rawName })
    = let protoName = toProtoName prefix rawName
      in EnumInfo protoName (toPath prefix rawName) (enumValues e)
  where enumValues :: D.EnumDescriptorProto -> [(EnumCode,String)]
        enumValues (D.EnumDescriptorProto.EnumDescriptorProto
                    { D.EnumDescriptorProto.value = value}) 
            = F.foldr ((:) . oneValue) [] value
          where oneValue  :: D.EnumValueDescriptorProto -> (EnumCode,String)
                oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                          { D.EnumValueDescriptorProto.name = Just name
                          , D.EnumValueDescriptorProto.number = Just number })
                    = (EnumCode number,toString name)

makeDescriptorInfo :: (ProtoName -> Seq FieldInfo)
                   -> String -> Bool -> D.DescriptorProto -> DescriptorInfo
makeDescriptorInfo getKnownKeys prefix isGroup
                   d@(D.DescriptorProto.DescriptorProto
                       { D.DescriptorProto.name = Just rawName
                       , D.DescriptorProto.field = rawFields
                       , D.DescriptorProto.extension_range = extension_range })
    = let di = DescriptorInfo protoName (toPath prefix rawName) isGroup
                              fieldInfos keyInfos extRangeList (getKnownKeys protoName)
      in di
  where protoName = toProtoName prefix rawName
        (fields,keys) = partition (\ f -> Nothing == (D.FieldDescriptorProto.extendee f)) . F.toList $ rawFields
        fieldInfos = Seq.fromList . map (toFieldInfo protoName) $ fields
        keyInfos = Seq.fromList . map (\f -> (keyExtendee prefix f,toFieldInfo protoName f)) $ keys
        extRangeList = concatMap check unchecked
          where check x@(lo,hi) | hi < lo = []
                                | hi<19000 || 19999<lo  = [x]
                                | otherwise = concatMap check [(lo,18999),(20000,hi)]
                unchecked = F.foldr ((:) . extToPair) [] extension_range
                extToPair (D.DescriptorProto.ExtensionRange
                            { D.DescriptorProto.ExtensionRange.start = start
                            , D.DescriptorProto.ExtensionRange.end = end }) =
                  (maybe minBound FieldId start, maybe maxBound FieldId end)
  
keyExtendee prefix f
    = case D.FieldDescriptorProto.extendee f of
        Nothing -> error "Impossible? keyExtendee expected Just but found Nothing"
        Just extName -> toProtoName prefix extName

toFieldInfo :: ProtoName ->  D.FieldDescriptorProto -> FieldInfo
toFieldInfo (ProtoName prefix mod parent)
            f@(D.FieldDescriptorProto.FieldDescriptorProto
                { D.FieldDescriptorProto.name = Just fieldName
                , D.FieldDescriptorProto.number = Just number
                , D.FieldDescriptorProto.label = Just label
                , D.FieldDescriptorProto.type' = Just type'
                , D.FieldDescriptorProto.type_name = mayTypeName
--                , D.FieldDescriptorProto.extendee = Nothing  -- sanity check
                , D.FieldDescriptorProto.default_value = mayRawDef })
    = fieldInfo
  where mayDef = parseDefaultValue f
        fieldInfo = let fullName = ProtoName prefix (dotPre mod parent) (toString fieldName)
                        fieldId = (FieldId (fromIntegral number))
                        fieldType = (FieldType (fromEnum type'))
                        wireTag = toWireTag fieldId fieldType
                        wireTagLength = size'Varint (getWireTag wireTag)
                    in FieldInfo fullName
                                 fieldId
                                 wireTag
                                 wireTagLength
                                 (label == LABEL_REQUIRED)
                                 (label == LABEL_REPEATED)
                                 fieldType
                                 (fmap (toProtoName prefix) mayTypeName)
                                 (fmap utf8 mayRawDef)
                                 mayDef

-- "Nothing" means no value specified
-- A failure to parse a provided value will result in an error at the moment
parseDefaultValue :: D.FieldDescriptorProto -> Maybe HsDefault
parseDefaultValue d@(D.FieldDescriptorProto.FieldDescriptorProto
                     { D.FieldDescriptorProto.type' = type'
                     , D.FieldDescriptorProto.default_value = mayDef })
    = do bs <- mayDef
         t <- type'
         todo <- case t of
                   TYPE_MESSAGE -> Nothing
                   TYPE_ENUM    -> Nothing
                   TYPE_GROUP   -> Nothing
                   TYPE_BOOL    -> Just parseDefBool
                   TYPE_BYTES   -> Just parseDefBytes
                   TYPE_DOUBLE  -> Just parseDefDouble
                   TYPE_FLOAT   -> Just parseDefFloat
                   TYPE_STRING  -> Just parseDefString
                   _            -> Just parseDefInteger
         case todo (utf8 bs) of
           Nothing -> error ("Could not parse the default value for "++show d)
           Just value -> return value

--- From here down is code used to parse the format of the default values in the .proto files

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

parseDefDouble :: ByteString -> Maybe HsDefault
parseDefDouble bs = fmap (HsDef'Rational . toRational) 
                    . mayRead reads' . U.toString $ bs
  where reads' :: ReadS Double
        reads' = readSigned' reads

parseDefFloat :: ByteString -> Maybe HsDefault
parseDefFloat bs = fmap  (HsDef'Rational . toRational) 
                   . mayRead reads' . U.toString $ bs
  where reads' :: ReadS Float
        reads' = readSigned' reads

parseDefString :: ByteString -> Maybe HsDefault
parseDefString bs = Just (HsDef'ByteString bs)

parseDefBytes :: ByteString -> Maybe HsDefault
parseDefBytes bs = Just (HsDef'ByteString bs)

parseDefInteger :: ByteString -> Maybe HsDefault
parseDefInteger bs = fmap HsDef'Integer . mayRead checkSign . U.toString $ bs
    where checkSign = readSigned' checkBase
          checkBase ('0':'x':xs) = readHex xs
          checkBase ('0':xs) = readOct xs
          checkBase xs = readDec xs

parseDefBool :: ByteString -> Maybe HsDefault
parseDefBool bs | bs == U.fromString "true" = Just (HsDef'Bool True)
                | bs == U.fromString "false" = Just (HsDef'Bool False)
                | otherwise = Nothing

-- The Numeric.readSigned does not handle '+' for some odd reason
readSigned' :: (Num a) => ([Char] -> [(a, t)]) -> [Char] -> [(a, t)]
readSigned' f ('-':xs) = map (\(v,s) -> (-v,s)) . f $ xs
readSigned' f ('+':xs) = f xs
readSigned' f xs = f xs
