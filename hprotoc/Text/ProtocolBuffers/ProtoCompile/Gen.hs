-- try "test", "testDesc", and "testLabel" to see sample output
-- 
-- Obsolete : Turn *Proto into Language.Haskell.Exts.Syntax from haskell-src-exts package
-- Now cut back to use just Language.Haskell.Syntax, see coments marked YYY for the Exts verision
-- 
-- Note that this may eventually also generate hs-boot files to allow
-- for breaking mutual recursion.  This is ignored for getting
-- descriptor.proto running.
--
-- Mangling: For the current moment, assume the mangling is done in a prior pass:
--   (*) Uppercase all module names and type names and enum constants
--   (*) lowercase all field names
--   (*) add a prime after all field names than conflict with reserved words
--
-- The names are also assumed to have become fully-qualified, and all
-- the optional type codes have been set.
--
-- default values are an awful mess.  They are documented in descriptor.proto as
{-
  // For numeric types, contains the original text representation of the value.
  // For booleans, "true" or "false".
  // For strings, contains the default text contents (not escaped in any way).
  // For bytes, contains the C escaped value.  All bytes >= 128 are escaped.
  // TODO(kenton):  Base-64 encode?
  optional string default_value = 7;
-}
module Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModule,enumModule,prettyPrint) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections(KeyInfo,HsDefault(..),DescriptorInfo(..),ProtoInfo(..),EnumInfo(..),ProtoName(..),ProtoFName(..),FieldInfo(..))

import qualified Data.ByteString.Lazy.Char8 as LC(unpack)
import qualified Data.Foldable as F(foldr,toList)
import Data.List(sortBy,foldl',foldl1')
import Data.Function(on)
import Language.Haskell.Pretty(prettyPrint)
import Language.Haskell.Syntax
import qualified Data.Map as M
import qualified Data.Sequence as Seq(null,length)
import qualified Data.Set as S

--import Debug.Trace(trace)

default (Int)

-- -- -- -- Helper functions

noWhere :: [HsDecl]
noWhere = [] -- YYY noWhere = (HsBDecls [])

($$) :: HsExp -> HsExp -> HsExp
($$) = HsApp

infixl 1 $$

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

litIntP :: Integral x => x -> HsPat
litIntP x | x<0 = HsPParen $ HsPLit (HsInt (toInteger x))
          | otherwise = HsPLit (HsInt (toInteger x))

litInt :: Integral x => x -> HsExp
litInt x | x<0 = HsParen $ HsLit (HsInt (toInteger x))
         | otherwise = HsLit (HsInt (toInteger x))

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (private s))

pvar :: String -> HsExp
pvar t = HsVar (private t)

lvar :: String -> HsExp
lvar t = HsVar (UnQual (HsIdent t))

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)

inst :: String -> [HsPat] -> HsExp -> HsDecl
inst s p r  = HsFunBind [HsMatch src (HsIdent s) p (HsUnGuardedRhs r) noWhere]

fqMod :: ProtoName -> String
fqMod (ProtoName _ a b c) = fmName $ foldr dotFM (promoteFM c) . map promoteFM $ a++b

joinMod :: [MName String] -> String
joinMod [] = ""
joinMod ms = fmName $ foldr1 dotFM . map promoteFM $ ms

baseIdent :: ProtoName -> HsName
baseIdent = HsIdent . mName . baseName
baseIdent' :: ProtoFName -> HsName
baseIdent' = HsIdent . fName . baseName'

qualName :: ProtoName -> HsQName
qualName p@(ProtoName _ _prefix [] _base) = UnQual (baseIdent p)
qualName p@(ProtoName _ _prefix (parents) _base) = Qual (Module (joinMod parents)) (baseIdent p)

qualFName :: ProtoFName -> HsQName
qualFName p@(ProtoFName _ _prefix [] _base) = UnQual (baseIdent' p)
qualFName p@(ProtoFName _ _prefix parents _base) = Qual (Module (joinMod parents)) (baseIdent' p)

unqualName :: ProtoName -> HsQName
unqualName p@(ProtoName _ _prefix _parent _base) = UnQual (baseIdent p)

mayQualName :: ProtoName -> ProtoFName -> HsQName
mayQualName (ProtoName _ c'prefix c'parents c'base) name@(ProtoFName _ prefix parents _base) =
  if joinMod (c'prefix++c'parents++[c'base]) == joinMod (prefix++parents)
    then UnQual (baseIdent' name) -- name is local, make UnQual
    else qualFName name           -- name is imported, make Qual

--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------
{-
enumModule :: String -> D.EnumDescriptorProto -> HsModule
enumModule prefix e
    = let ei = makeEnumInfo prefix e
-}
enumModule :: EnumInfo -> HsModule
enumModule ei
    = let protoName = enumName ei
      in HsModule src (Module (fqMod protoName))
           (Just [HsEThingAll (UnQual (baseIdent protoName))])
           (standardImports False) (enumDecls ei)

enumDecls :: EnumInfo -> [HsDecl]
enumDecls ei =  map ($ ei) [ enumX
                           , instanceMergeableEnum
                           , instanceBounded
                           , instanceDefaultEnum
                           , instanceEnum
                           , instanceWireEnum
                           , instanceGPB . enumName
                           , instanceMessageAPI . enumName
                           , instanceReflectEnum
                           ]

enumX :: EnumInfo -> HsDecl
enumX ei = HsDataDecl src [] (baseIdent (enumName ei)) [] (map enumValueX (enumValues ei)) derivesEnum
  where enumValueX (_,name) = HsConDecl src (HsIdent name) []

instanceMergeableEnum :: EnumInfo -> HsDecl
instanceMergeableEnum ei 
  = HsInstDecl src [] (private "Mergeable") [HsTyCon (unqualName (enumName ei))] []

instanceBounded :: EnumInfo -> HsDecl
instanceBounded ei
    = HsInstDecl src [] (private "Bounded") [HsTyCon (unqualName (enumName ei))] 
        [set "minBound" (head values),set "maxBound" (last values)] -- values cannot be null in a well formed enum
  where values = enumValues ei
        set f (_,n) = inst f [] (HsCon (UnQual (HsIdent n)))

{- from google's descriptor.h, about line 346:

  // Get the field default value if cpp_type() == CPPTYPE_ENUM.  If no
  // explicit default was defined, the default is the first value defined
  // in the enum type (all enum types are required to have at least one value).
  // This never returns NULL.

-}
instanceDefaultEnum :: EnumInfo -> HsDecl
instanceDefaultEnum ei
    = HsInstDecl src [] (private "Default") [HsTyCon (unqualName (enumName ei))]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: HsExp
        firstValue = case enumValues ei of
                       (:) (_,n) _ -> HsCon (UnQual (HsIdent n))
                       [] -> error $ "Impossible? EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show ei

instanceEnum :: EnumInfo -> HsDecl
instanceEnum ei
    = HsInstDecl src [] (private "Enum") [HsTyCon (unqualName (enumName ei))]
        (map HsFunBind [fromEnum',toEnum',succ',pred'])
  where values = enumValues ei
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = HsMatch src (HsIdent "fromEnum") [HsPApp (UnQual (HsIdent n)) []]
                               (HsUnGuardedRhs (litInt (getEnumCode v))) noWhere
        toEnum' = map toEnum'one values
        toEnum'one (v,n) = HsMatch src (HsIdent "toEnum") [litIntP (getEnumCode v)] -- enums cannot be negative so no parenthesis are required to protect a negative sign
                             (HsUnGuardedRhs (HsCon (UnQual (HsIdent n)))) noWhere
        succ' = zipWith (equate "succ") values (tail values)
        pred' = zipWith (equate "pred") (tail values) values
        equate f (_,n1) (_,n2) = HsMatch src (HsIdent f) [HsPApp (UnQual (HsIdent n1)) []]
                                   (HsUnGuardedRhs (HsCon (UnQual (HsIdent n2)))) noWhere

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: EnumInfo -> HsDecl
instanceWireEnum ei
    = HsInstDecl src [] (private "Wire") [HsTyCon (unqualName (enumName ei))]
        [ withName "wireSize", withName "wirePut", withGet, withGetErr ]
  where withName foo = inst foo [HsPVar (HsIdent "ft'"),HsPVar (HsIdent "enum")] rhs
          where rhs = pvar foo $$ lvar "ft'" $$
                        (HsParen $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [litIntP 14] rhs
          where rhs = pvar "fmap" $$ pvar "toEnum" $$
                        (HsParen $ pvar "wireGet" $$ HsLit (HsInt 14))
        withGetErr = inst "wireGet" [HsPVar (HsIdent "ft'")] rhs
          where rhs = pvar "wireGetErr" $$ lvar "ft'"

instanceGPB :: ProtoName -> HsDecl
instanceGPB protoName
    = HsInstDecl src [] (private "GPB") [HsTyCon (unqualName protoName)] []

instanceReflectEnum :: EnumInfo -> HsDecl
instanceReflectEnum ei
    = HsInstDecl src [] (private "ReflectEnum") [HsTyCon (unqualName (enumName ei))]
        [ inst "reflectEnum" [] ascList
        , inst "reflectEnumInfo" [ HsPWildCard ] ei' ]
  where (ProtoName xxx a b c) = enumName ei
        xxx'Exp = HsParen $ pvar "pack" $$ HsLit (HsString (LC.unpack (utf8 (fiName xxx))))
        values = enumValues ei
        ascList,ei',protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,ns) = HsTuple [litInt (getEnumCode v),HsLit (HsString ns),HsCon (UnQual (HsIdent ns))]
        ei' = foldl' HsApp (HsCon (private "EnumInfo")) [protoNameExp
                                                        ,HsList $ map (HsLit . HsString) (enumFilePath ei)
                                                        ,HsList (map two values)]
          where two (v,ns) = HsTuple [litInt (getEnumCode v),HsLit (HsString ns)]
        protoNameExp = HsParen $ foldl' HsApp (HsVar (private "makePNF")) [ xxx'Exp, mList a, mList b, HsLit (HsString (mName c)) ]
          where mList = HsList . map (HsLit . HsString . mName)

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

protoModule :: ProtoInfo -> ByteString -> HsModule
protoModule pri@(ProtoInfo protoName _ _ keyInfos _ _ _) fdpBS
  = let exportKeys = map (HsEVar . UnQual . baseIdent' . fieldName . snd) (F.toList keyInfos)
        exportNames = map (HsEVar . UnQual . HsIdent) ["protoInfo","fileDescriptorProto"]
        imports = protoImports ++ map formatImport (protoImport pri)
    in HsModule src (Module (fqMod protoName)) (Just (exportKeys++exportNames)) imports (keysX protoName keyInfos ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
  where protoImports = standardImports (not . Seq.null . extensionKeys $ pri) ++
                       [ HsImportDecl src (Module "Text.DescriptorProtos.FileDescriptorProto") False Nothing
                           (Just (False,[HsIAbs (HsIdent "FileDescriptorProto")]))
                       , HsImportDecl src (Module "Text.ProtocolBuffers.Reflections") False Nothing
                           (Just (False,[HsIAbs (HsIdent "ProtoInfo")]))
                       , HsImportDecl src (Module "Text.ProtocolBuffers.WireMessage") True (Just (Module "P'"))
                           (Just (False,[HsIVar (HsIdent "wireGet,getFromBS")]))
                       ]
        formatImport ((a,b),s) = HsImportDecl src (Module a) True asM (Just (False,map (HsIAbs . HsIdent) (S.toList s)))
          where asM | a==b = Nothing
                    | otherwise = Just (Module b)

protoImport :: ProtoInfo -> [((String,String),S.Set String)]
protoImport protoInfo
    = M.assocs . M.fromListWith S.union . filter isForeign . map withMod $ keyNames
  where isForeign = let here = fqMod protoName
                    in (\((a,_),_) -> a/=here)
        protoName = protoMod protoInfo
        withMod p@(ProtoName _ _prefix modname base) = ((fqMod p,joinMod modname),S.singleton (mName base))
        keyNames = F.foldr (\(e,fi) rest -> e : addName fi rest) [] (extensionKeys protoInfo)
        addName fi rest = maybe rest (:rest) (typeName fi)

embed'ProtoInfo :: ProtoInfo -> [HsDecl]
embed'ProtoInfo pri = [ myType, myValue ]
  where myType = HsTypeSig src [ HsIdent "protoInfo" ] (HsQualType [] (HsTyCon (UnQual (HsIdent "ProtoInfo"))))
        myValue = HsPatBind src (HsPApp (UnQual (HsIdent "protoInfo")) []) (HsUnGuardedRhs $
                    pvar "read" $$ HsLit (HsString (show pri))) noWhere

embed'fdpBS :: ByteString -> [HsDecl]
embed'fdpBS bs = [ myType, myValue ]
  where myType = HsTypeSig src [ HsIdent "fileDescriptorProto" ] (HsQualType [] (HsTyCon (UnQual (HsIdent "FileDescriptorProto"))))
        myValue = HsPatBind src (HsPApp (UnQual (HsIdent "fileDescriptorProto")) []) (HsUnGuardedRhs $
                    pvar "getFromBS" $$
                      HsParen (pvar "wireGet" $$ litInt 11) $$ 
                      HsParen (pvar "pack" $$ HsLit (HsString (LC.unpack bs)))) noWhere

descriptorModule :: DescriptorInfo -> HsModule
descriptorModule di
    = let protoName = descName di
          un = UnQual . baseIdent $ protoName
          imports = standardImports (hasExt di) ++ map formatImport (toImport di)
          exportKeys = map (HsEVar . UnQual . baseIdent' . fieldName . snd) (F.toList (keys di))
          formatImport ((a,b),s) = HsImportDecl src (Module a) True asM (Just (False, map (HsIAbs . HsIdent) (S.toList s)))
            where asM | a==b = Nothing
                      | otherwise = Just (Module b)
      in HsModule src (Module (fqMod protoName))
           (Just (HsEThingAll un : exportKeys))
           imports (descriptorX di : (keysX protoName (keys di) ++ instancesDescriptor di))

standardImports :: Bool -> [HsImportDecl]
standardImports ext =
  [ HsImportDecl src (Module "Prelude") False Nothing (Just (False,ops))
  , HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]
 where ops | ext = map (HsIVar . HsSymbol) ["+","<=","&&"," || "]
           | otherwise = map (HsIVar . HsSymbol) ["+"]

toImport :: DescriptorInfo -> [((String,String),S.Set String)]
toImport di
    = M.assocs . M.fromListWith S.union . filter isForeign . map withMod $ allNames
  where isForeign = let here = fqMod protoName
                    in (\((a,_),_) -> a/=here)
        protoName = descName di
        withMod (Left p@(ProtoName _ _prefix modname base)) = ((fqMod p,joinMod modname),S.singleton (mName base))
        withMod (Right (ProtoFName _ prefix modname base)) = ((joinMod (prefix++modname),joinMod modname),S.singleton (fName base))
        allNames = F.foldr addName keyNames (fields di)
        keyNames = F.foldr (\(e,fi) rest -> Left e : addName fi rest) keysKnown (keys di)
        addName fi rest = maybe rest (:rest) (fmap Left (typeName fi))
        keysKnown = F.foldr (\fi rest -> Right (fieldName fi) : rest) [] (knownKeys di)

keysX :: ProtoName -> Seq KeyInfo -> [HsDecl]
keysX self i = concatMap (makeKey self) . F.toList $ i

makeKey :: ProtoName -> KeyInfo -> [HsDecl]
makeKey self (extendee,f) = [ keyType, keyVal ]
  where keyType = HsTypeSig src [ baseIdent' . fieldName $ f ] (HsQualType [] (foldl1 HsTyApp . map HsTyCon $
                    [ private "Key", private labeled
                    , if extendee /= self then qualName extendee else unqualName extendee
                    , typeQName ]))
        labeled | canRepeat f = "Seq"
                | otherwise = "Maybe"
        typeNumber = getFieldType . typeCode $ f
        typeQName :: HsQName
        typeQName = case useType typeNumber of
                      Just s -> private s
                      Nothing -> case typeName f of
                                   Just s | self /= s -> qualName s
                                          | otherwise -> unqualName s
                                   Nothing -> error $  "No Name for Field!\n" ++ show f
        keyVal = HsPatBind src (HsPApp (UnQual (baseIdent' . fieldName $ f)) []) (HsUnGuardedRhs
                   (pvar "Key" $$ litInt (getFieldId (fieldNumber f))
                               $$ litInt typeNumber
                               $$ maybe (pvar "Nothing") (HsParen . (pvar "Just" $$) . (defToSyntax (typeCode f))) (hsDefault f)
                   )) noWhere

defToSyntax :: FieldType -> HsDefault -> HsExp
defToSyntax tc x =
  case x of
    HsDef'Bool b -> HsCon (private (show b))
    HsDef'ByteString bs -> (if tc == 9 then (\xx -> HsParen (pvar "Utf8" $$ xx)) else id) $
                           (HsParen $ pvar "pack" $$ HsLit (HsString (LC.unpack bs)))
    HsDef'Rational r | r < 0 -> HsParen $ HsLit (HsFrac r)
                     | otherwise -> HsLit (HsFrac r)
    HsDef'Integer i | i < 0 -> HsParen $ HsLit (HsInt i)
                    | otherwise -> HsLit (HsInt i)
    HsDef'Enum s -> HsParen $ pvar "read" $$ HsLit (HsString s)

descriptorX :: DescriptorInfo -> HsDecl
descriptorX di = HsDataDecl src [] name [] [con] derives
  where self = descName di
        name = baseIdent $ self
        con = HsRecDecl src name eFields
                where eFields = F.foldr ((:) . fieldX) end (fields di)
                      end = (if hasExt di then (extfield:) else id) 
                          $ (if storeUnknown di then [unknownField] else [])
        extfield :: ([HsName],HsBangType)
        extfield = ([HsIdent "ext'field"],HsUnBangedTy (HsTyCon (Qual (Module "P'") (HsIdent "ExtField"))))
        unknownField :: ([HsName],HsBangType)
        unknownField = ([HsIdent "unknown'field"],HsUnBangedTy (HsTyCon (Qual (Module "P'") (HsIdent "UnknownField"))))
        fieldX :: FieldInfo -> ([HsName],HsBangType)
        fieldX fi = ([baseIdent' . fieldName $ fi],HsUnBangedTy (labeled (HsTyCon typed)))
          where labeled | canRepeat fi = typeApp "Seq"
                        | isRequired fi = id
                        | otherwise = typeApp "Maybe"
                typed :: HsQName
                typed = case useType (getFieldType (typeCode fi)) of
                          Just s -> private s
                          Nothing -> case typeName fi of
                                       Just s | self /= s -> qualName s
                                              | otherwise -> unqualName s
                                       Nothing -> error $  "No Name for Field!\n" ++ show fi

instancesDescriptor :: DescriptorInfo -> [HsDecl]
instancesDescriptor di = map ($ di) $
   (if hasExt di then (instanceExtendMessage:) else id) $
   (if storeUnknown di then (instanceUnknownMessage:) else id) $
   [ instanceMergeable
   , instanceDefault
   , instanceWireDescriptor
   , instanceMessageAPI . descName
   , instanceGPB . descName                 
   , instanceReflectDescriptor
   ]

instanceExtendMessage :: DescriptorInfo -> HsDecl
instanceExtendMessage di
    = HsInstDecl src [] (private "ExtendMessage") [HsTyCon (UnQual (baseIdent (descName di)))]
        [ inst "getExtField" [] (lvar "ext'field")
        , inst "putExtField" [HsPVar (HsIdent "e'f"),HsPVar (HsIdent "msg")] putextfield
        , inst "validExtRanges" [ HsPVar (HsIdent "msg") ] (pvar "extRanges" $$ (HsParen $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (UnQual (HsIdent "ext'field")) (lvar "e'f") ]

instanceUnknownMessage :: DescriptorInfo -> HsDecl
instanceUnknownMessage di
    = HsInstDecl src [] (private "UnknownMessage") [HsTyCon (UnQual (baseIdent (descName di)))]
        [ inst "getUnknownField" [] (lvar "unknown'field")
        , inst "putUnknownField" [HsPVar (HsIdent "u'f"),HsPVar (HsIdent "msg")] putunknownfield
        ]
  where putunknownfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (UnQual (HsIdent "unknown'field")) (lvar "u'f") ]

instanceMergeable :: DescriptorInfo -> HsDecl
instanceMergeable di
    = HsInstDecl src [] (private "Mergeable") [HsTyCon un]
        [ inst "mergeEmpty" [] (foldl' HsApp (HsCon un) (replicate len (HsCon (private "mergeEmpty"))))
        , inst "mergeAppend" [HsPApp un patternVars1, HsPApp un patternVars2]
                             (foldl' HsApp (HsCon un) (zipWith append vars1 vars2))
        ]
  where un = UnQual (baseIdent (descName di))
        len = (if hasExt di then succ else id)
            $ (if storeUnknown di then succ else id)
            $ Seq.length (fields di)
        patternVars1,patternVars2 :: [HsPat]
        patternVars1 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("x'" ++ show n))) [1..]
        patternVars2 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("y'" ++ show n))) [1..]
        vars1,vars2 :: [HsExp]
        vars1 = take len inf
            where inf = map (\n -> lvar ("x'" ++ show n)) [1..]
        vars2 = take len inf
            where inf = map (\n -> lvar ("y'" ++ show n)) [1..]
        append x y = HsParen $ pvar "mergeAppend" $$ x $$ y

instanceDefault :: DescriptorInfo -> HsDecl
instanceDefault di
    = HsInstDecl src [] (private "Default") [HsTyCon un]
        [ inst "defaultValue" [] (foldl' HsApp (HsCon un) deflistExt) ]
  where un = UnQual (baseIdent (descName di))
        deflistExt = F.foldr ((:) . defX) end (fields di)
        end = (if hasExt di then (pvar "defaultValue":) else id) 
            $ (if storeUnknown di then [pvar "defaultValue"] else [])

        defX :: FieldInfo -> HsExp
        defX fi | isRequired fi = dv1
                | otherwise = dv2
          where dv1 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> defToSyntax (typeCode fi) hsdef
                dv2 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> HsParen $ HsCon (private "Just") $$ defToSyntax (typeCode fi) hsdef

instanceMessageAPI :: ProtoName -> HsDecl
instanceMessageAPI protoName
    = HsInstDecl src [] (private "MessageAPI") [HsTyVar (HsIdent "msg'"), HsTyFun (HsTyVar (HsIdent "msg'")) (HsTyCon un),  (HsTyCon un)]
        [ inst "getVal" [HsPVar (HsIdent "m'"),HsPVar (HsIdent "f'")] (HsApp (lvar "f'" ) (lvar "m'")) ]
  where un = UnQual (baseIdent protoName)

mkOp :: String -> HsExp -> HsExp -> HsExp
mkOp s a b = HsInfixApp a (HsQVarOp (UnQual (HsSymbol s))) b

instanceWireDescriptor :: DescriptorInfo -> HsDecl
instanceWireDescriptor di@(DescriptorInfo { descName = protoName
                                          , fields = fieldInfos
                                          , extRanges = allowedExts
                                          , knownKeys = fieldExts })
  = let me = unqualName protoName
        extensible = not (null allowedExts)
        len = (if extensible then succ else id) 
            $ (if storeUnknown di then succ else id)
            $ Seq.length fieldInfos
        mine = HsPApp me . take len . map (\n -> HsPVar (HsIdent ("x'" ++ show n))) $ [1..]
        vars = take len . map (\n -> lvar ("x'" ++ show n)) $ [1..]
        mExt | extensible = Just (vars !! Seq.length fieldInfos)
             | otherwise = Nothing
        mUnknown | storeUnknown di = Just (last vars)
                 | otherwise = Nothing

        -- first case is for Group behavior, second case is for Message behavior, last is error handler
        cases g m e = HsCase (lvar "ft'") [ HsAlt src (litIntP 10) (HsUnGuardedAlt g) noWhere
                                          , HsAlt src (litIntP 11) (HsUnGuardedAlt m) noWhere
                                          , HsAlt src HsPWildCard (HsUnGuardedAlt e) noWhere
                                          ]

        sizeCases = HsUnGuardedRhs $ cases (lvar "calc'Size") 
                                           (pvar "prependMessageSize" $$ lvar "calc'Size")
                                           (pvar "wireSizeErr" $$ lvar "ft'" $$ lvar "self'")
        whereCalcSize = [HsFunBind [HsMatch src (HsIdent "calc'Size") [] (HsUnGuardedRhs sizes) noWhere]]
        sizes | null sizesList = HsLit (HsInt 0)
              | otherwise = HsParen (foldl1' (+!) sizesList)
          where (+!) = mkOp "+"
                sizesList | Just v <- mUnknown = sizesListExt ++ [ pvar "wireSizeUnknownField" $$ v ]
                          | otherwise = sizesListExt
                sizesListExt | Just v <- mExt = sizesListFields ++ [ pvar "wireSizeExtField" $$ v ] 
                             | otherwise = sizesListFields
                sizesListFields =  zipWith toSize vars . F.toList $ fieldInfos
        toSize var fi = let f = if isRequired fi then "wireSizeReq"
                                  else if canRepeat fi then "wireSizeRep"
                                      else "wireSizeOpt"
                        in foldl' HsApp (pvar f) [ litInt (wireTagLength fi)
                                                 , litInt (getFieldType (typeCode fi))
                                                 , var]

        putCases = HsUnGuardedRhs $ cases
          (lvar "put'Fields")
          (HsDo [ HsQualifier $ pvar "putSize" $$
                    (HsParen $ foldl' HsApp (pvar "wireSize") [ litInt 10 , lvar "self'" ])
                , HsQualifier $ lvar "put'Fields" ])
          (pvar "wirePutErr" $$ lvar "ft'" $$ lvar "self'")
        wherePutFields = [HsFunBind [HsMatch src (HsIdent "put'Fields") [] (HsUnGuardedRhs (HsDo putStmts)) noWhere]]
        putStmts = putStmtsContent
          where putStmtsContent | null putStmtsAll = [HsQualifier $ pvar "return" $$ HsCon (Special HsUnitCon)]
                                | otherwise = putStmtsAll
                putStmtsAll | Just v <- mUnknown = putStmtsListExt ++ [ HsQualifier $ pvar "wirePutUnknownField" $$ v ]
                             | otherwise = putStmtsListExt
                putStmtsListExt | Just v <- mExt = sortedPutStmtsList ++ [ HsQualifier $ pvar "wirePutExtField" $$ v ]
                                | otherwise = sortedPutStmtsList
                sortedPutStmtsList = map snd                                          -- remove number
                                     . sortBy (compare `on` fst)                      -- sort by number
                                     . zip (map fieldNumber . F.toList $ fieldInfos)  -- add number as fst
                                     $ putStmtsList
                putStmtsList = zipWith toPut vars . F.toList $ fieldInfos
        toPut var fi = let f = if isRequired fi then "wirePutReq"
                                 else if canRepeat fi then "wirePutRep"
                                     else "wirePutOpt"
                       in HsQualifier $
                          foldl' HsApp (pvar f) [ litInt (getWireTag (wireTag fi))
                                                , litInt (getFieldType (typeCode fi))
                                                , var]

        getCases = HsUnGuardedRhs $ cases
          (pvar "getBareMessageWith" $$ otherField $$ lvar "update'Self")
          (pvar "getMessageWith" $$ otherField $$ lvar "update'Self")
          (pvar "wireGetErr" $$ lvar "ft'")
        whereDecls | extensible = [whereUpdateSelf,processExt]
                   | otherwise  = [whereUpdateSelf]
        whereUpdateSelf = HsFunBind [HsMatch src (HsIdent "update'Self")
                           [HsPVar (HsIdent "field'Number") ,HsPVar (HsIdent "old'Self")]
                           (HsUnGuardedRhs (HsCase (lvar "field'Number") updateAlts)) noWhere]
        otherField | extensible = lvar "other'Field"
                   | otherwise = processUnknown
        processUnknown | storeUnknown di = pvar "loadUnknown"
                       | otherwise = pvar "unknown"
        processExt =
          HsFunBind [HsMatch src (HsIdent "other'Field")
                       [HsPVar (HsIdent "field'Number"), HsPVar (HsIdent "wire'Type"), HsPVar (HsIdent "old'Self")]
                       (HsUnGuardedRhs (HsParen (HsIf (isAllowed (lvar "field'Number"))
                                                      (pvar "loadExtension")
                                                      (processUnknown))
                                        $$ lvar "field'Number" $$ lvar "wire'Type" $$ lvar "old'Self")) noWhere]
        isAllowed x = pvar "or" $$ HsList ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&";
          ranges = map (\(FieldId lo,FieldId hi) -> if hi < maxHi then (litInt lo <=! x) &&! (x <=! litInt hi)
                                                      else litInt lo <=! x) allowedExts
             where FieldId maxHi = maxBound
        updateAlts = map toUpdate (F.toList fieldInfos)
                     ++ (if extensible && (not (Seq.null fieldExts)) then map toUpdateExt (F.toList fieldExts) else [])
                     ++ [HsAlt src HsPWildCard (HsUnGuardedAlt $
                           pvar "unknownField" $$ (lvar "field'Number")) noWhere]
        toUpdateExt fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $
                           pvar "wireGetKey" $$ HsVar (mayQualName protoName (fieldName fi)) $$ lvar "old'Self") noWhere
        -- fieldIds cannot be negative so no parenthesis are required to protect a negative sign
        toUpdate fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $ 
                        pvar "fmap" $$ (HsParen $ HsLambda src [HsPVar (HsIdent "new'Field")] $
                                          HsRecUpdate (lvar "old'Self") [HsFieldUpdate (UnQual . baseIdent' . fieldName $ fi)
                                                                                       (labelUpdate fi)])
                                    $$ (HsParen (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ HsParen ((lvar . fName . baseName' . fieldName $ fi) $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (HsCon (private "Just") $$ lvar "new'Field")
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,11] =
                               pvar "mergeAppend" $$ HsParen ((lvar . fName . baseName' . fieldName $ fi) $$ lvar "old'Self") $$ (HsParen x)
                           | otherwise = x

    in HsInstDecl src [] (private "Wire") [HsTyCon me]
        [ HsFunBind [HsMatch src (HsIdent "wireSize") [HsPVar (HsIdent "ft'"),HsPAsPat (HsIdent "self'") (HsPParen mine)] sizeCases whereCalcSize]
        , HsFunBind [HsMatch src (HsIdent "wirePut")  [HsPVar (HsIdent "ft'"),HsPAsPat (HsIdent "self'") (HsPParen mine)] putCases wherePutFields]
        , HsFunBind [HsMatch src (HsIdent "wireGet") [HsPVar (HsIdent "ft'")] getCases whereDecls]
        ]

-- TODO : Encode allow as a proper set of numbers!
instanceReflectDescriptor :: DescriptorInfo -> HsDecl
instanceReflectDescriptor di
    = HsInstDecl src [] (private "ReflectDescriptor") [HsTyCon (UnQual (baseIdent (descName di)))]
        [ inst "reflectDescriptorInfo" [ HsPWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: HsExp
        rdi = pvar "read" $$ HsLit (HsString (show di))

------------------------------------------------------------------

derives,derivesEnum :: [HsQName]
derives = map private ["Show","Eq","Ord","Typeable"]
derivesEnum = map private ["Read","Show","Eq","Ord","Typeable"]

useType :: Int -> Maybe String
useType  1 = Just "Double"
useType  2 = Just "Float"
useType  3 = Just "Int64"
useType  4 = Just "Word64"
useType  5 = Just "Int32"
useType  6 = Just "Word64"
useType  7 = Just "Word32"
useType  8 = Just "Bool"
useType  9 = Just "Utf8"
useType 10 = Nothing
useType 11 = Nothing
useType 12 = Just "ByteString"
useType 13 = Just "Word32"
useType 14 = Nothing
useType 15 = Just "Int32"
useType 16 = Just "Int64"
useType 17 = Just "Int32"
useType 18 = Just "Int64"
useType  x = error $ "Text.ProtocolBuffers.Gen: Impossible? useType Unknown type code "++show x
