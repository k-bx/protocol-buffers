-- | The 'MakeReflections' module takes the 'FileDescriptorProto'
-- output from 'Resolve' and produces a 'ProtoInfo' from
-- 'Reflections'.  This also takes a Haskell module prefix and the
-- proto's package namespace as input.  The output is suitable
-- for passing to the 'Gen' module to produce the files.
--
-- This acheives several things: It moves the data from a nested tree
-- to flat lists and maps. It moves the group information from the
-- parent Descriptor to the actual Descriptor.  It moves the data out
-- of Maybe types.  It converts Utf8 to String.  Keys known to extend
-- a Descriptor are listed in that Descriptor.
-- 
-- In building the reflection info new things are computed. It changes
-- dotted names to ProtoName with the outer prefix.  It parses the
-- default value from the ByteString to a Haskell type.  For fields,
-- the value of the tag on the wire is computed and so is its size on
-- the wire.
module Text.ProtocolBuffers.ProtoCompile.MakeReflections(makeProtoInfo,serializeFDP) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..)) 
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import           Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto(FileDescriptorProto)) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage(size'Varint,toWireTag,runPut)
import Text.ProtocolBuffers.ProtoCompile.Resolve(ReMap,NameMap(..))

import qualified Data.Foldable as F(foldr,toList)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString,toString)
import qualified Data.Sequence as Seq(fromList,empty,singleton,null)
import Numeric(readHex,readOct,readDec)
import Data.Monoid(mconcat,mappend)
import qualified Data.Map as M(fromListWith,lookup,keys)
import Data.Maybe(fromMaybe,catMaybes,fromJust)
import System.FilePath

--import Debug.Trace (trace)

imp :: String -> a
imp msg = error $ "Text.ProtocolBuffers.ProtoCompile.MakeReflections: Impossible?\n  "++msg

pnPath :: ProtoName -> [FilePath]
pnPath (ProtoName _ a b c) = splitDirectories .flip addExtension "hs" . joinPath . map mName $ a++b++[c]

serializeFDP :: D.FileDescriptorProto -> ByteString
serializeFDP fdp = runPut (wirePut 11 fdp)

toHaskell :: ReMap -> FIName Utf8 -> ProtoName
toHaskell reMap k = case M.lookup k reMap of
                      Nothing -> imp $ "toHaskell failed to find "++show k++" among "++show (M.keys reMap)
                      Just pn -> pn

makeProtoInfo :: Bool -> NameMap -> D.FileDescriptorProto -> ProtoInfo
makeProtoInfo unknownField (NameMap (packageName,hPrefix,hParent) reMap)
              fdp@(D.FileDescriptorProto { D.FileDescriptorProto.name = Just rawName })
     = ProtoInfo protoName (pnPath protoName) (toString rawName) keyInfos allMessages allEnums allKeys where
  protoName = case hParent of
                [] -> case hPrefix of
                        [] -> imp $ "makeProtoInfo: no hPrefix or hParent in NameMap for: "++show fdp
                        _ -> ProtoName packageName (init hPrefix) [] (last hPrefix)
                _ -> ProtoName packageName hPrefix (init hParent) (last hParent)
  keyInfos = Seq.fromList . map (\f -> (keyExtendee' reMap f,toFieldInfo' reMap packageName f))
             . F.toList . D.FileDescriptorProto.extension $ fdp
  allMessages = concatMap (processMSG packageName False) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allEnums = map (makeEnumInfo' reMap packageName) (F.toList $ D.FileDescriptorProto.enum_type fdp) 
             ++ concatMap (processENM packageName) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allKeys = M.fromListWith mappend . map (\(k,a) -> (k,Seq.singleton a))
            . F.toList . mconcat $ keyInfos : map keys allMessages
  processMSG parent msgIsGroup msg = 
    let getKnownKeys protoName' = fromMaybe Seq.empty (M.lookup protoName' allKeys)
        groups = collectedGroups msg
        checkGroup x = elem (fromMaybe (imp $ "no message name in makeProtoInfo.processMSG.checkGroup:\n"++show msg)
                                       (D.DescriptorProto.name x))
                            groups
        parent' = fqAppend parent [IName (fromJust (D.DescriptorProto.name msg))]
    in makeDescriptorInfo' reMap parent getKnownKeys msgIsGroup unknownField msg
       : concatMap (\x -> processMSG parent' (checkGroup x) x)
                   (F.toList (D.DescriptorProto.nested_type msg))
  processENM parent msg = foldr ((:) . makeEnumInfo' reMap parent') nested
                          (F.toList (D.DescriptorProto.enum_type msg))
    where parent' = fqAppend parent [IName (fromJust (D.DescriptorProto.name msg))]
          nested = concatMap (processENM parent') (F.toList (D.DescriptorProto.nested_type msg))
makeProtoInfo _ _ _ = imp $ "makeProtoInfo: missing name or package"

makeEnumInfo' :: ReMap -> FIName Utf8 -> D.EnumDescriptorProto -> EnumInfo
makeEnumInfo' reMap parent
              e@(D.EnumDescriptorProto.EnumDescriptorProto
                  { D.EnumDescriptorProto.name = Just rawName
                  , D.EnumDescriptorProto.value = value })
    = if Seq.null value then imp $ "enum has no values: "++show e
        else EnumInfo protoName (pnPath protoName) enumVals
  where protoName = toHaskell reMap $ fqAppend parent [IName rawName]
        enumVals ::[(EnumCode,String)]
        enumVals = F.foldr ((:) . oneValue) [] value
          where oneValue :: D.EnumValueDescriptorProto -> (EnumCode,String)
                oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                          { D.EnumValueDescriptorProto.name = Just name
                          , D.EnumValueDescriptorProto.number = Just number })
                    = (EnumCode number,mName . baseName . toHaskell reMap $ fqAppend (protobufName protoName) [IName name])
                oneValue evdp = imp $ "no name or number for evdp passed to makeEnumInfo.oneValue: "++show evdp
makeEnumInfo' _ _ _ = imp "makeEnumInfo: missing name"

keyExtendee' :: ReMap -> D.FieldDescriptorProto.FieldDescriptorProto -> ProtoName
keyExtendee' reMap f = case D.FieldDescriptorProto.extendee f of
                         Nothing -> imp $ "keyExtendee expected Just but found Nothing: "++show f
                         Just extName -> toHaskell reMap (FIName extName)

makeDescriptorInfo' :: ReMap -> FIName Utf8
                    -> (ProtoName -> Seq FieldInfo)
                    -> Bool -> Bool
                    -> D.DescriptorProto -> DescriptorInfo
makeDescriptorInfo' reMap parent getKnownKeys msgIsGroup unknownField
                    (D.DescriptorProto.DescriptorProto
                      { D.DescriptorProto.name = Just rawName
                      , D.DescriptorProto.field = rawFields
                      , D.DescriptorProto.extension = rawKeys
                      , D.DescriptorProto.extension_range = extension_range })
    = let di = DescriptorInfo protoName (pnPath protoName) msgIsGroup
                              fieldInfos keyInfos extRangeList (getKnownKeys protoName)
                              unknownField
      in di -- trace (toString rawName ++ "\n" ++ show di ++ "\n\n") $ di
  where protoName = toHaskell reMap $ fqAppend parent [IName rawName]
        fieldInfos = fmap (toFieldInfo' reMap (protobufName protoName)) rawFields
        keyInfos = fmap (\f -> (keyExtendee' reMap f,toFieldInfo' reMap (protobufName protoName) f)) rawKeys
        extRangeList = concatMap check unchecked
          where check x@(lo,hi) | hi < lo = []
                                | hi<19000 || 19999<lo  = [x]
                                | otherwise = concatMap check [(lo,18999),(20000,hi)]
                unchecked = F.foldr ((:) . extToPair) [] extension_range
                extToPair (D.DescriptorProto.ExtensionRange
                            { D.DescriptorProto.ExtensionRange.start = mStart
                            , D.DescriptorProto.ExtensionRange.end = mEnd }) =
                  (maybe minBound FieldId mStart, maybe maxBound (FieldId . pred) mEnd)
makeDescriptorInfo' _ _ _ _ _ _ = imp $ "makeDescriptorInfo: missing name"

toFieldInfo' :: ReMap -> FIName Utf8 -> D.FieldDescriptorProto -> FieldInfo
toFieldInfo' reMap parent
             f@(D.FieldDescriptorProto.FieldDescriptorProto
                 { D.FieldDescriptorProto.name = Just name
                 , D.FieldDescriptorProto.number = Just number
                 , D.FieldDescriptorProto.label = Just label
                 , D.FieldDescriptorProto.type' = Just type'
                 , D.FieldDescriptorProto.type_name = mayTypeName
                 , D.FieldDescriptorProto.default_value = mayRawDef })
    = fieldInfo
  where mayDef = parseDefaultValue f
        fieldInfo = let (ProtoName x a b c) = toHaskell reMap $ fqAppend parent [IName name]
                        protoFName = ProtoFName x a b (mangle c)
                        fieldId = (FieldId (fromIntegral number))
                        fieldType = (FieldType (fromEnum type'))
                        wt = toWireTag fieldId fieldType
                        wtLength = size'Varint (getWireTag wt)
                    in FieldInfo protoFName
                                 fieldId
                                 wt
                                 wtLength
                                 (label == LABEL_REQUIRED)
                                 (label == LABEL_REPEATED)
                                 fieldType
                                 (fmap (toHaskell reMap . FIName) mayTypeName)
                                 (fmap utf8 mayRawDef)
                                 mayDef
toFieldInfo' _ _ f = imp $ "toFieldInfo: missing info in "++show f

collectedGroups :: D.DescriptorProto -> [Utf8] 
collectedGroups = catMaybes
                . map D.FieldDescriptorProto.type_name
                . filter (\f -> D.FieldDescriptorProto.type' f == Just TYPE_GROUP) 
                . F.toList
                . D.DescriptorProto.field

{-
spanEndL :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
spanEndL f bs = let (a,b) = SC.spanEnd f (S.concat . LC.toChunks $ bs)
                in (LC.fromChunks [a],LC.fromChunks [b])

-- Take a bytestring of "A" into "Right A" and "A.B.C" into "Left (A.B,C)"
splitMod :: Utf8 -> Either (Utf8,Utf8) Utf8
splitMod (Utf8 bs) = case spanEndL ('.'/=) bs of
                       (pre,post) | LC.length pre <= 1 -> Right (Utf8 bs)
                                  | otherwise -> Left (Utf8 (LC.init pre),Utf8 post)

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
dotPre s ('.':x) = dotPre s x
dotPre "" x = x
dotPre s x | '.' == last s = dotPre (init s) x
dotPre s "" = s
dotPre s x = s ++ ( '.' : x )

preDot :: String -> String
preDot [] = []
preDot xs@('.':_) = xs
preDot xs = '.':xs


toProtoName :: String -> Utf8 -> ProtoName
toProtoName prefix rawName =
  case splitMod rawName of
    Left (m,b) -> ProtoName xxx prefix (toString m) (toString b)
    Right b    -> ProtoName xxx prefix ""           (toString b)


makeProtoInfo :: Bool -> String -> [String] -> D.FileDescriptorProto -> ProtoInfo
makeProtoInfo unknownField prefix names
              fdp@(D.FileDescriptorProto { D.FileDescriptorProto.name = Just rawName })
     = ProtoInfo protoName (pnPath protoName) (toString rawName) keyInfos allMessages allEnums allKeys where
  protoName = case names of
                [] -> ProtoName xxx prefix "" "" -- after this the value of names cannot be []
                [name] -> ProtoName xxx prefix "" name
                _ -> ProtoName xxx prefix (foldr1 (\a bs -> a  ++ ('.' : bs)) (init names)) (last names) -- names cannot be []
  keyInfos = Seq.fromList . map (\f -> (keyExtendee prefix f,toFieldInfo protoName f))
             . F.toList . D.FileDescriptorProto.extension $ fdp
  allMessages = concatMap (processMSG False) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allEnums = map (makeEnumInfo prefix) (F.toList $ D.FileDescriptorProto.enum_type fdp) 
             ++ concatMap processENM (F.toList $ D.FileDescriptorProto.message_type fdp)
  allKeys = M.fromListWith mappend . map (\(k,a) -> (k,Seq.singleton a))
            . F.toList . mconcat $ keyInfos : map keys allMessages

  processMSG msgIsGroup msg = 
    let getKnownKeys protoName' = fromMaybe Seq.empty (M.lookup protoName' allKeys)
        groups = collectedGroups msg
        checkGroup x = elem (fromMaybe (imp $ "no message name in makeProtoInfo.processMSG.checkGroup:\n"++show msg)
                                       (D.DescriptorProto.name x))
                            groups
    in makeDescriptorInfo getKnownKeys prefix msgIsGroup unknownField msg
       : concatMap (\x -> processMSG (checkGroup x) x)
                   (F.toList (D.DescriptorProto.nested_type msg))
  processENM msg = foldr ((:) . makeEnumInfo prefix) nested
                         (F.toList (D.DescriptorProto.enum_type msg))
    where nested = concatMap processENM (F.toList (D.DescriptorProto.nested_type msg))
makeProtoInfo unknownField prefix names fdp = imp $ "no name in fdp passed to makeProtoInfo: " ++ show (unknownField,prefix,names,fdp)

makeEnumInfo :: String -> D.EnumDescriptorProto -> EnumInfo
makeEnumInfo prefix e@(D.EnumDescriptorProto.EnumDescriptorProto
                        { D.EnumDescriptorProto.name = Just rawName
                        , D.EnumDescriptorProto.value = value })
    = let protoName = toProtoName prefix rawName
      in if Seq.null value then imp $ "enum has no values: "++show (prefix,e)
           else EnumInfo protoName (toPath prefix rawName) enumVals
  where enumVals ::[(EnumCode,String)]
        enumVals = F.foldr ((:) . oneValue) [] value
          where oneValue  :: D.EnumValueDescriptorProto -> (EnumCode,String)
                oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                          { D.EnumValueDescriptorProto.name = Just name
                          , D.EnumValueDescriptorProto.number = Just number })
                    = (EnumCode number,toString name)
                oneValue evdp = imp $ "no name or number for evdp passed to makeEnumInfo.oneValue: "++show evdp
makeEnumInfo prefix e = imp $ "no name for enum passed to makeEnumInfo: " ++ show (prefix,e)

makeDescriptorInfo :: (ProtoName -> Seq FieldInfo)
                   -> String -> Bool -> Bool
                   -> D.DescriptorProto -> DescriptorInfo
makeDescriptorInfo getKnownKeys prefix msgIsGroup unknownField
                   (D.DescriptorProto.DescriptorProto
                     { D.DescriptorProto.name = Just rawName
                     , D.DescriptorProto.field = rawFields
                     , D.DescriptorProto.extension_range = extension_range })
    = let di = DescriptorInfo protoName (toPath prefix rawName) msgIsGroup
                              fieldInfos keyInfos extRangeList (getKnownKeys protoName)
                              unknownField
      in di -- trace (toString rawName ++ "\n" ++ show di ++ "\n\n") $ di
  where protoName = toProtoName prefix rawName
        (msgFields,keysHere) = partition (\ f -> Nothing == (D.FieldDescriptorProto.extendee f)) . F.toList $ rawFields
        fieldInfos = Seq.fromList . map (toFieldInfo protoName) $ msgFields
        keyInfos = Seq.fromList . map (\f -> (keyExtendee prefix f,toFieldInfo protoName f)) $ keysHere
        extRangeList = concatMap check unchecked
          where check x@(lo,hi) | hi < lo = []
                                | hi<19000 || 19999<lo  = [x]
                                | otherwise = concatMap check [(lo,18999),(20000,hi)]
                unchecked = F.foldr ((:) . extToPair) [] extension_range
                extToPair (D.DescriptorProto.ExtensionRange
                            { D.DescriptorProto.ExtensionRange.start = start
                            , D.DescriptorProto.ExtensionRange.end = end }) =
                  (maybe minBound FieldId start, maybe maxBound (FieldId . pred) end)
makeDescriptorInfo _ prefix msgIsGroup unknownField d =
  imp $ "No name passed in dp passed to makeDescriptorInfo: "++show (prefix,msgIsGroup,unknownField,d)

keyExtendee :: String -> D.FieldDescriptorProto.FieldDescriptorProto -> ProtoName
keyExtendee prefix f
    = case D.FieldDescriptorProto.extendee f of
        Nothing -> imp $ "keyExtendee expected Just but found Nothing: "++show (prefix,f)
        Just extName -> toProtoName prefix extName

toFieldInfo :: ProtoName ->  D.FieldDescriptorProto -> FieldInfo
toFieldInfo (ProtoName xxx prefix modName parent)
            f@(D.FieldDescriptorProto.FieldDescriptorProto
                { D.FieldDescriptorProto.name = Just name
                , D.FieldDescriptorProto.number = Just number
                , D.FieldDescriptorProto.label = Just label
                , D.FieldDescriptorProto.type' = Just type'
                , D.FieldDescriptorProto.type_name = mayTypeName
--                , D.FieldDescriptorProto.extendee = Nothing  -- sanity check
                , D.FieldDescriptorProto.default_value = mayRawDef })
    = fieldInfo
  where mayDef = parseDefaultValue f
        fieldInfo = let fullName = ProtoName xxx prefix (dotPre modName parent) (toString name)
                        fieldId = (FieldId (fromIntegral number))
                        fieldType = (FieldType (fromEnum type'))
                        wt = toWireTag fieldId fieldType
                        wtLength = size'Varint (getWireTag wt)
                    in FieldInfo fullName
                                 fieldId
                                 wt
                                 wtLength
                                 (label == LABEL_REQUIRED)
                                 (label == LABEL_REPEATED)
                                 fieldType
                                 (fmap (toProtoName prefix) mayTypeName)
                                 (fmap utf8 mayRawDef)
                                 mayDef
toFieldInfo pn f = imp $ "Not enough information defined in field passed to toFieldInfo: "++show(pn,f)
-}
-- "Nothing" means no value specified
-- A failure to parse a provided value will result in an error at the moment
parseDefaultValue :: D.FieldDescriptorProto -> Maybe HsDefault
parseDefaultValue f@(D.FieldDescriptorProto.FieldDescriptorProto
                     { D.FieldDescriptorProto.type' = type'
                     , D.FieldDescriptorProto.default_value = mayRawDef })
    = do bs <- mayRawDef
         t <- type'
         todo <- case t of
                   TYPE_MESSAGE -> Nothing
                   TYPE_GROUP   -> Nothing
                   TYPE_ENUM    -> Just parseDefEnum
                   TYPE_BOOL    -> Just parseDefBool
                   TYPE_BYTES   -> Just parseDefBytes
                   TYPE_DOUBLE  -> Just parseDefDouble
                   TYPE_FLOAT   -> Just parseDefFloat
                   TYPE_STRING  -> Just parseDefString
                   _            -> Just parseDefInteger
         case todo (utf8 bs) of
           Nothing -> error $ "Could not parse as type "++ show t ++"the default value "++ show mayRawDef ++" for field "++show f
           Just value -> return value

--- From here down is code used to parse the format of the default values in the .proto files

parseDefEnum :: ByteString -> Maybe HsDefault
parseDefEnum = Just . HsDef'Enum . U.toString

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
