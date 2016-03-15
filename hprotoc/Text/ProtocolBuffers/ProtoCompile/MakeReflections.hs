{-# LANGUAGE OverloadedStrings #-}
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
-- dotted names to ProtoName using the translator from
-- 'makeNameMaps'.  It parses the default value from the ByteString to
-- a Haskell type.  For fields, the value of the tag on the wire is
-- computed and so is its size on the wire.
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
-- import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import           Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
-- import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions(FieldOptions))
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto(FileDescriptorProto))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.OneofDescriptorProto           as D(OneofDescriptorProto)
import qualified Text.DescriptorProtos.OneofDescriptorProto           as D.OneofDescriptorProto(OneofDescriptorProto(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage(size'WireTag,toWireTag,toPackedWireTag,runPut,Wire(..))
import Text.ProtocolBuffers.ProtoCompile.Resolve(ReMap,NameMap(..),getPackageID)

-- import Text.ProtocolBuffers.Reflections

import qualified Data.Foldable as F(foldr,toList,notElem)
import           Data.Sequence ((<|),(><))
import qualified Data.Sequence as Seq
import Numeric(readHex,readOct,readDec)
import Data.Monoid(mconcat,mappend)
import qualified Data.Map as M(fromListWith,lookup,keys)
import Data.Maybe(fromMaybe,catMaybes,fromJust,isJust)
import System.FilePath

import qualified Data.List as List
import qualified Data.ByteString.Lazy.Char8 as BL8

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

makeProtoInfo :: (Bool,Bool,Bool,Bool) -- unknownField, lazyFields, lenses and json for makeDescriptorInfo'
              -> NameMap
              -> D.FileDescriptorProto
              -> ProtoInfo
makeProtoInfo (unknownField,lazyFieldsOpt,lenses,json) (NameMap (packageID,hPrefix,hParent) reMap)
              fdp@(D.FileDescriptorProto { D.FileDescriptorProto.name = Just rawName })
     = ProtoInfo protoName (pnPath protoName) (toString rawName) keyInfos allMessages allEnums allOneofs allKeys where
  packageName = getPackageID packageID :: FIName (Utf8)
  protoName = case hParent of
                [] -> case hPrefix of
                        [] -> imp $ "makeProtoInfo: no hPrefix or hParent in NameMap for: "++show fdp
                        _ -> ProtoName packageName (init hPrefix) [] (last hPrefix)
                _ -> ProtoName packageName hPrefix (init hParent) (last hParent)
  keyInfos = Seq.fromList . map (\f -> (keyExtendee' reMap f,toFieldInfo' reMap packageName lenses Nothing f))
             . F.toList . D.FileDescriptorProto.extension $ fdp
  allMessages = concatMap (processMSG packageName False) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allEnums = map (makeEnumInfo' reMap packageName json) (F.toList $ D.FileDescriptorProto.enum_type fdp)
             ++ concatMap (processENM packageName) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allOneofs = concatMap (processONO packageName) (F.toList $ D.FileDescriptorProto.message_type fdp)
  allKeys = M.fromListWith mappend . map (\(k,a) -> (k,Seq.singleton a))
            . F.toList . mconcat $ keyInfos : map keys allMessages
  processMSG parent msgIsGroup msg =
    let getKnownKeys protoName' = fromMaybe Seq.empty (M.lookup protoName' allKeys)
        groups = collectedGroups msg
        checkGroup x = elem (fromMaybe (imp $ "no message name in makeProtoInfo.processMSG.checkGroup:\n"++show msg)
                                       (D.DescriptorProto.name x))
                            groups
        parent' = fqAppend parent [IName (fromJust (D.DescriptorProto.name msg))]
    in makeDescriptorInfo' reMap parent getKnownKeys msgIsGroup (unknownField,lazyFieldsOpt,lenses,json) msg
       : concatMap (\x -> processMSG parent' (checkGroup x) x)
                   (F.toList (D.DescriptorProto.nested_type msg))
  processENM parent msg = foldr ((:) . makeEnumInfo' reMap parent' json) nested
                          (F.toList (D.DescriptorProto.enum_type msg))
    where parent' = fqAppend parent [IName (fromJust (D.DescriptorProto.name msg))]
          nested = concatMap (processENM parent') (F.toList (D.DescriptorProto.nested_type msg))
  processONO parent msg = foldr ((:) . makeOneofInfo' reMap parent' lenses msg) nested
                          (zip [0..] (F.toList (D.DescriptorProto.oneof_decl msg)))
    where parent' = fqAppend parent [IName (fromJust (D.DescriptorProto.name msg))]
          nested = concatMap (processONO parent') (F.toList (D.DescriptorProto.nested_type msg))
makeProtoInfo _ _ _ = imp $ "makeProtoInfo: missing name or package"

makeEnumInfo' :: ReMap -> FIName Utf8 -> Bool -> D.EnumDescriptorProto -> EnumInfo
makeEnumInfo' reMap parent json
              e@(D.EnumDescriptorProto.EnumDescriptorProto
                  { D.EnumDescriptorProto.name = Just rawName
                  , D.EnumDescriptorProto.value = value })
    = if Seq.null value then imp $ "enum has no values: "++show e
        else EnumInfo protoName (pnPath protoName) enumVals json
  where protoName = toHaskell reMap $ fqAppend parent [IName rawName]
        enumVals ::[(EnumCode,String)]
        enumVals = F.foldr ((:) . oneValue) [] value
          where oneValue :: D.EnumValueDescriptorProto -> (EnumCode,String)
                oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                          { D.EnumValueDescriptorProto.name = Just name
                          , D.EnumValueDescriptorProto.number = Just number })
                    = (EnumCode number,mName . baseName . toHaskell reMap $ fqAppend (protobufName protoName) [IName name])
                oneValue evdp = imp $ "no name or number for evdp passed to makeEnumInfo.oneValue: "++show evdp
makeEnumInfo' _ _ _ _ = imp "makeEnumInfo: missing name"

makeOneofInfo' :: ReMap -> FIName Utf8
               -> Bool -- ^ makeLenses
               -> D.DescriptorProto -> (Int32,D.OneofDescriptorProto) -> OneofInfo
makeOneofInfo' reMap parent lenses parentProto
              (n, D.OneofDescriptorProto.OneofDescriptorProto
                  { D.OneofDescriptorProto.name = Just rawName })
    = OneofInfo protoName protoFName (pnPath protoName) fieldInfos lenses
  where protoName@(ProtoName x a b c) = toHaskell reMap $ fqAppend parent [IName rawName]
        protoFName = ProtoFName x a b (mangle c) (if lenses then "_" else "")
        rawFields = D.DescriptorProto.field parentProto
        rawFieldsOneof = Seq.filter ((== Just n) . D.FieldDescriptorProto.oneof_index) rawFields
        getFieldProtoName fdp
          = case D.FieldDescriptorProto.name fdp of
              Just name -> toHaskell reMap $ fqAppend (protobufName protoName) [IName name]
              Nothing -> imp $ "getFieldProtoName: missing info in " ++ show fdp
        getFieldInfo = toFieldInfo' reMap (protobufName protoName) lenses Nothing
        fieldInfos = fmap (\f -> (getFieldProtoName f, getFieldInfo f)) rawFieldsOneof
makeOneofInfo' _ _ _ _ _ = imp "makeOneofInfo: missing name"


keyExtendee' :: ReMap -> D.FieldDescriptorProto.FieldDescriptorProto -> ProtoName
keyExtendee' reMap f = case D.FieldDescriptorProto.extendee f of
                         Nothing -> imp $ "keyExtendee expected Just but found Nothing: "++show f
                         Just extName -> toHaskell reMap (FIName extName)
                           -- let debugMsg = unlines [ "MakeReflections.keyExtendee'.debugMsg"
                           --                        , "extName is " ++ show extName
                           --                        , "reMap is :"
                           --                        , show reMap ]

makeDescriptorInfo' :: ReMap -> FIName Utf8
                    -> (ProtoName -> Seq FieldInfo)
                    -> Bool -- msgIsGroup
                    -> (Bool,Bool,Bool,Bool) -- unknownField, lazyFields, lenses, json
                    -> D.DescriptorProto -> DescriptorInfo
makeDescriptorInfo' reMap parent getKnownKeys msgIsGroup (unknownField,lazyFieldsOpt,lenses,json)
                    msg@(D.DescriptorProto.DescriptorProto
                      { D.DescriptorProto.name = Just rawName
                      , D.DescriptorProto.field = rawFields
                      , D.DescriptorProto.oneof_decl = rawOneofs
                      , D.DescriptorProto.extension = rawKeys
                      , D.DescriptorProto.extension_range = extension_range })
    = DescriptorInfo
        { descName     = protoName
        , descFilePath = pnPath protoName
        , isGroup      = msgIsGroup
        , fields       = fieldInfos
        , descOneofs   = oneofInfos
        , keys         = keyInfos
        , extRanges    = extRangeList
        , knownKeys    = getKnownKeys protoName
        , storeUnknown = unknownField
        , lazyFields   = lazyFieldsOpt
        , makeLenses   = lenses
        , jsonInstances = json
        , mapEntry     = case D.DescriptorProto.options msg of
                            Just D.MessageOptions.MessageOptions{
                                D.MessageOptions.map_entry = Just True
                            } -> True
                            _ -> False
        }
  where protoName = toHaskell reMap $ fqAppend parent [IName rawName]
        rawFieldsNotOneof =
            -- and not oneof
            Seq.filter (\x -> D.FieldDescriptorProto.oneof_index x == Nothing) $
            -- not map
            Seq.filter (`F.notElem` mapFields) rawFields

        fieldInfos =
            fmap (toFieldInfo' reMap (protobufName protoName) lenses Nothing) rawFieldsNotOneof
            ><
            fmap (\x ->
                toFieldInfo' reMap (protobufName protoName) lenses (findFieldKV x) x) mapFields

        oneofInfos = F.foldr ((<|) . makeOneofInfo' reMap (protobufName protoName) lenses msg) Seq.empty
                          (zip [0..] (F.toList rawOneofs))

        keyInfos = fmap (\f -> (keyExtendee' reMap f,toFieldInfo' reMap (protobufName protoName) lenses Nothing f)) rawKeys

        extRangeList = concatMap check unchecked
          where check x@(lo,hi) | hi < lo = []
                                | hi<19000 || 19999<lo  = [x]
                                | otherwise = concatMap check [(lo,18999),(20000,hi)]
                unchecked = F.foldr ((:) . extToPair) [] extension_range
                extToPair (D.DescriptorProto.ExtensionRange
                            { D.DescriptorProto.ExtensionRange.start = mStart
                            , D.DescriptorProto.ExtensionRange.end = mEnd }) =
                  (maybe minBound FieldId mStart, maybe maxBound (FieldId . pred) mEnd)

        -- | Given a DescriptorProto for map entry, extract types of "key" and
        -- "value" fields
        kvTypes
            :: D.DescriptorProto.DescriptorProto
            -> ((FieldType, Maybe ProtoName), (FieldType, Maybe ProtoName))
        kvTypes dp =
            let fields_ = F.toList $ D.DescriptorProto.field dp in
            -- key, value fields
            let Just kf = List.find
                    (\fdp -> D.FieldDescriptorProto.name fdp == Just (Utf8 "key"))
                    fields_
            in
            let Just vf = List.find
                    (\fdp -> D.FieldDescriptorProto.name fdp == Just (Utf8 "value"))
                    fields_
            in
            -- key, value field infos
            let kfi = toFieldInfo' reMap (protobufName protoName) lenses Nothing kf in
            let vfi = toFieldInfo' reMap (protobufName protoName) lenses Nothing vf in
            -- key, value type code and type names
            let ktc = (typeCode kfi, typeName kfi) in
            let vtc = (typeCode vfi, typeName vfi) in
            (ktc, vtc)

        -- | Given a FieldDescriptorProto of a map field find the corresponding
        -- DescriptorProto of the _Entry module and extract the types of its
        -- "key" and "value" fields
        findFieldKV
            :: D.FieldDescriptorProto.FieldDescriptorProto
            -> Maybe ((FieldType, Maybe ProtoName), (FieldType, Maybe ProtoName))
        findFieldKV fdp =
            let getType (Utf8 x) = Utf8 $ last (BL8.split '.' x) in
            fmap kvTypes $
                List.find
                    (\dp_ ->
                        D.DescriptorProto.name dp_
                        ==
                        (getType `fmap` D.FieldDescriptorProto.type_name fdp)
                    )
                    mapEntries

        -- map field entry (k,v) wrappers
        mapEntries :: [D.DescriptorProto.DescriptorProto]
        mapEntries =
            -- convert to list because it's easier to search
            F.toList $
            Seq.filter
                (\x -> case x of
                    D.DescriptorProto.DescriptorProto {
                       D.DescriptorProto.options =
                            Just D.MessageOptions.MessageOptions{
                                D.MessageOptions.map_entry = Just True
                            }
                    } -> True
                    _ -> False
                )
                (D.DescriptorProto.nested_type msg)

        -- There is nothing in FieldDescriptorProto to distinguish a map field
        -- (since they are just sequences). Hence, find all fields such that
        -- the DescriptorProtos of their types have map_entry option set
        mapFields :: Seq D.FieldDescriptorProto
        mapFields =
            Seq.filter
                (\fld ->
                    let getType (Utf8 f) = Utf8 $ last (BL8.split '.' f) in
                    let tn = D.FieldDescriptorProto.type_name fld in
                    let xs =
                            List.filter
                                (\y -> D.DescriptorProto.name y == (getType `fmap` tn))
                                mapEntries
                    in
                    not (List.null xs)
                )
                rawFields
makeDescriptorInfo' _ _ _ _ _ _ = imp $ "makeDescriptorInfo: missing name"

toFieldInfo'
  :: ReMap
  -> FIName Utf8
  -> Bool -- ^ whether to use lences (if True, an underscore prefix is used)
  -- if the field is a map field: (k,v) types
  -> Maybe ((FieldType, Maybe ProtoName), (FieldType, Maybe ProtoName))
  -> D.FieldDescriptorProto
  -> FieldInfo
toFieldInfo' reMap parent lenses mapKV
             f@(D.FieldDescriptorProto.FieldDescriptorProto
                 { D.FieldDescriptorProto.name = Just name
                 , D.FieldDescriptorProto.number = Just number
                 , D.FieldDescriptorProto.label = Just label
                 , D.FieldDescriptorProto.type' = Just type'
                 , D.FieldDescriptorProto.type_name = mayTypeName
                 , D.FieldDescriptorProto.default_value = mayRawDef
                 , D.FieldDescriptorProto.options = mayOpt })
    = fieldInfo
  where mayDef = parseDefaultValue f
        fieldInfo = let (ProtoName x a b c) = toHaskell reMap $ fqAppend parent [IName name]
                        protoFName = ProtoFName x a b (mangle c) (if lenses then "_" else "")
                        fieldId = (FieldId (fromIntegral number))
                        fieldType = (FieldType (fromEnum type'))
{- removed to update 1.5.5 to be compatible with protobuf-2.3.0
                        wt | packedOption = toPackedWireTag fieldId
                           | otherwise = toWireTag fieldId fieldType
-}
                        wt | packedOption = toPackedWireTag fieldId                -- write packed
                           | otherwise = toWireTag fieldId fieldType               -- write unpacked

                        wt2 | validPacked = Just (toWireTag fieldId fieldType      -- read unpacked
                                                 ,toPackedWireTag fieldId)         -- read packed
                            | otherwise = Nothing
                        wtLength = size'WireTag wt
                        packedOption = case mayOpt of
                                         Just (D.FieldOptions { D.FieldOptions.packed = Just True }) -> True
                                         _ -> False
                        validPacked = isValidPacked label fieldType
                    in FieldInfo protoFName
                                 fieldId
                                 wt
                                 wt2
                                 wtLength
                                 packedOption
                                 (label == LABEL_REQUIRED)
                                 (label == LABEL_REPEATED)
                                 validPacked
                                 fieldType
                                 (fmap (toHaskell reMap . FIName) mayTypeName)
                                 (fmap utf8 mayRawDef)
                                 mayDef
                                 (isJust mapKV) {- isMapField -}
                                 mapKV          {- (k,v) types -}
toFieldInfo' _ _ _ _ f = imp $ "toFieldInfo: missing info in "++show f

collectedGroups :: D.DescriptorProto -> [Utf8]
collectedGroups = catMaybes
                . map D.FieldDescriptorProto.type_name
                . filter (\f -> D.FieldDescriptorProto.type' f == Just TYPE_GROUP)
                . F.toList
                . D.DescriptorProto.field

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
         case todo bs of
           Nothing -> error $ "Could not parse as type "++ show t ++" the default value (raw) is "++ show mayRawDef ++" for field "++show f
           Just value -> return value

--- From here down is code used to parse the format of the default values in the .proto files

-- On 25 August 2010 20:12, George van den Driessche <georgevdd@google.com> sent Chris Kuklewicz a
-- patch to MakeReflections.parseDefEnum to ensure that HsDef'Enum holds the mangled form of the
-- name.
parseDefEnum :: Utf8 -> Maybe HsDefault
parseDefEnum = Just . HsDef'Enum . mName . mangle . IName . uToString

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

parseDefDouble :: Utf8 -> Maybe HsDefault
parseDefDouble bs = case (uToString bs) of
                      "nan" -> Just (HsDef'RealFloat SRF'nan)
                      "-inf" -> Just (HsDef'RealFloat SRF'ninf)
                      "inf" -> Just (HsDef'RealFloat SRF'inf)
                      s -> fmap (HsDef'RealFloat . SRF'Rational . toRational) . mayRead reads'$ s
  where reads' :: ReadS Double
        reads' = readSigned' reads

{-
parseDefDouble :: Utf8 -> Maybe HsDefault
parseDefDouble bs |
                  | otherwise = fmap (HsDef'Rational . toRational)
                                . mayRead reads' . uToString $ bs
-}


parseDefFloat :: Utf8 -> Maybe HsDefault
parseDefFloat bs = case (uToString bs) of
                      "nan" -> Just (HsDef'RealFloat SRF'nan)
                      "-inf" -> Just (HsDef'RealFloat SRF'ninf)
                      "inf" -> Just (HsDef'RealFloat SRF'inf)
                      s -> fmap (HsDef'RealFloat . SRF'Rational . toRational) . mayRead reads'$ s
  where reads' :: ReadS Float
        reads' = readSigned' reads

{-
parseDefFloat :: Utf8 -> Maybe HsDefault
parseDefFloat bs = fmap  (HsDef'Rational . toRational)
                   . mayRead reads' . uToString $ bs
  where reads' :: ReadS Float
        reads' = readSigned' reads
-}

parseDefString :: Utf8 -> Maybe HsDefault
parseDefString bs = Just (HsDef'ByteString (utf8 bs))

parseDefBytes :: Utf8 -> Maybe HsDefault
parseDefBytes bs = Just (HsDef'ByteString (utf8 bs))

parseDefInteger :: Utf8 -> Maybe HsDefault
parseDefInteger bs = fmap HsDef'Integer . mayRead checkSign . uToString $ bs
    where checkSign = readSigned' checkBase
          checkBase ('0':'x':xs@(_:_)) = readHex xs
          checkBase ('0':xs@(_:_)) = readOct xs
          checkBase xs = readDec xs

parseDefBool :: Utf8 -> Maybe HsDefault
parseDefBool bs | bs == uFromString "true" = Just (HsDef'Bool True)
                | bs == uFromString "false" = Just (HsDef'Bool False)
                | otherwise = Nothing

-- The Numeric.readSigned does not handle '+' for some odd reason
readSigned' :: (Num a) => ([Char] -> [(a, t)]) -> [Char] -> [(a, t)]
readSigned' f ('-':xs) = map (\(v,s) -> (-v,s)) . f $ xs
readSigned' f ('+':xs) = f xs
readSigned' f xs = f xs

-- Must keep synchronized with Parser.isValidPacked
isValidPacked :: Label -> FieldType -> Bool
isValidPacked LABEL_REPEATED fieldType =
  case fieldType of
    9 -> False
    10 -> False
    11 -> False -- Impossible value for typeCode from parseType, but here for completeness
    12 -> False
    _ -> True
isValidPacked _ _ = False
