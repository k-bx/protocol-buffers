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
--   (*) Uppercase all module names and type names
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
module Text.ProtocolBuffers.Gen(protoModule,descriptorModule,enumModule,prettyPrint) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Reflections(KeyInfo,HsDefault(..),DescriptorInfo(..),ProtoInfo(..),EnumInfo(..),ProtoName(..),FieldInfo(..))

import qualified Data.ByteString.Lazy.Char8 as LC(unpack)
import Data.Char(isUpper)
import qualified Data.Foldable as F(foldr,toList)
import Data.List(sort,group,foldl',foldl1')
import Language.Haskell.Pretty(prettyPrint)
import Language.Haskell.Syntax
import qualified Data.Map as M
import qualified Data.Sequence as Seq(null,length)
import qualified Data.Set as S

import Debug.Trace(trace)

-- -- -- -- Helper functions

noWhere :: [HsDecl]
noWhere = [] -- YYY noWhere = (HsBDecls [])

($$) :: HsExp -> HsExp -> HsExp
($$) = HsApp

infixl 1 $$

dotPre :: String -> String -> String
dotPre "" x = x
dotPre x "" = x
dotPre s x@('.':xs)  | '.' == last s = s ++ xs
                     | otherwise = s ++ x
dotPre s x | '.' == last s = s++x
           | otherwise = s++('.':x)

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

fqName :: ProtoName -> String
fqName (ProtoName a b c) = dotPre a (dotPre b c)

qualName :: ProtoName -> HsQName
qualName (ProtoName _prefix "" base) = UnQual (HsIdent base)
qualName (ProtoName _prefix parent base) = Qual (Module parent) (HsIdent base)

unqualName :: ProtoName -> HsQName
unqualName (ProtoName _prefix _parent base) = UnQual (HsIdent base)

mayQualName :: ProtoName -> ProtoName -> HsQName
mayQualName context name@(ProtoName prefix modname base) =
  if fqName context == dotPre prefix modname then UnQual (HsIdent base)
    else qualName name

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
      in HsModule src (Module (fqName protoName))
           (Just [HsEThingAll (UnQual (HsIdent (baseName protoName)))])
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
enumX ei = HsDataDecl src [] (HsIdent (baseName (enumName ei))) [] (map enumValueX (enumValues ei)) derivesEnum
  where enumValueX (_,name) = HsConDecl src (HsIdent name) []

instanceMergeableEnum :: EnumInfo -> HsDecl
instanceMergeableEnum ei 
  = HsInstDecl src [] (private "Mergeable") [HsTyCon (unqualName (enumName ei))] []

instanceBounded :: EnumInfo -> HsDecl
instanceBounded ei
    = HsInstDecl src [] (private "Bounded") [HsTyCon (unqualName (enumName ei))] 
        [set "minBound" (head values),set "maxBound" (last values)]
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
        [ withName "wireSize", withName "wirePut", withGet ]
  where withName foo = inst foo [litIntP 14,HsPVar (HsIdent "enum")] rhs
          where rhs = (pvar foo $$ HsLit (HsInt 14)) $$
                      (HsParen $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [litIntP 14] rhs
          where rhs = (pvar "fmap" $$ pvar "toEnum") $$
                      (HsParen $ pvar "wireGet" $$ HsLit (HsInt 14))

instanceGPB :: ProtoName -> HsDecl
instanceGPB protoName
    = HsInstDecl src [] (private "GPB") [HsTyCon (unqualName protoName)] []

instanceReflectEnum :: EnumInfo -> HsDecl
instanceReflectEnum ei
    = HsInstDecl src [] (private "ReflectEnum") [HsTyCon (unqualName (enumName ei))]
        [ inst "reflectEnum" [] ascList
        , inst "reflectEnumInfo" [ HsPWildCard ] ei' ]
  where (ProtoName a b c) = enumName ei
        values = enumValues ei
        ascList,ei',protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,ns) = HsTuple [litInt (getEnumCode v),HsLit (HsString ns),HsCon (UnQual (HsIdent ns))]
        ei' = foldl' HsApp (HsCon (private "EnumInfo")) [protoNameExp
                                                        ,HsList $ map (HsLit . HsString) (enumFilePath ei)
                                                        ,HsList (map two values)]
          where two (v,ns) = HsTuple [litInt (getEnumCode v),HsLit (HsString ns)]
        protoNameExp = HsParen $ foldl' HsApp (HsCon (private "ProtoName")) . map (HsLit . HsString) $ [a,b,c]

--------------------------------------------
-- DescriptorProto module creation is unfinished
--   There are difficult namespace issues
--------------------------------------------

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

protoModule :: ProtoInfo -> ByteString -> HsModule
protoModule pri@(ProtoInfo protoName _ _ keyInfos _ _ _) fdpBS
  = let exportKeys = map (HsEVar . UnQual . HsIdent . baseName . fieldName . snd) (F.toList keyInfos)
        exportNames = map (HsEVar . UnQual . HsIdent) ["protoInfo","fileDescriptorProto"]
        imports = protoImports ++ map formatImport (protoImport pri)
    in HsModule src (Module (fqName protoName)) (Just (exportKeys++exportNames)) imports (keysX protoName keyInfos ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
  where protoImports = [ HsImportDecl src (Module "Text.DescriptorProtos.FileDescriptorProto") False Nothing
                           (Just (False,[HsIAbs (HsIdent "FileDescriptorProto")]))
                       , HsImportDecl src (Module "Text.ProtocolBuffers.Reflections") False Nothing
                           (Just (False,[HsIAbs (HsIdent "ProtoInfo")]))
                       , HsImportDecl src (Module "Text.ProtocolBuffers.WireMessage") False Nothing
                           (Just (False,[HsIVar (HsIdent "wireGet,getFromBS")]))
                       , HsImportDecl src (Module "Data.ByteString.Lazy.Char8") False Nothing
                           (Just (False,[HsIAbs (HsIdent "pack")]))
                       ]
        formatImport (m,t) = HsImportDecl src (Module (dotPre (haskellPrefix protoName) (dotPre m t))) True
                               (Just (Module m)) (Just (False,[HsIAbs (HsIdent t)]))

protoImport :: ProtoInfo -> [(String,String)]
protoImport (ProtoInfo protoName _ _ keyInfos _ _ _)
    = map head . group . sort 
      . filter (selfName /=)
      . concatMap withMod
      $ allNames
  where selfName = (parentModule protoName, baseName protoName)
        withMod (ProtoName _prefix "" _base) = []
        withMod (ProtoName _prefix modname base) = [(modname,base)]
        allNames = F.foldr (\(e,fi) rest -> e : addName fi rest) [] keyInfos
        addName fi rest = maybe rest (:rest) (typeName fi)

embed'ProtoInfo :: ProtoInfo -> [HsDecl]
embed'ProtoInfo pri = [ myType, myValue ]
  where myType = HsTypeSig src [ HsIdent "protoInfo" ] (HsQualType [] (HsTyCon (UnQual (HsIdent "ProtoInfo"))))
        myValue = HsPatBind src (HsPApp (UnQual (HsIdent "protoInfo")) []) (HsUnGuardedRhs $
                    lvar "read" $$ HsLit (HsString (show pri))) noWhere

embed'fdpBS :: ByteString -> [HsDecl]
embed'fdpBS bs = [ myType, myValue ]
  where myType = HsTypeSig src [ HsIdent "fileDescriptorProto" ] (HsQualType [] (HsTyCon (UnQual (HsIdent "FileDescriptorProto"))))
        myValue = HsPatBind src (HsPApp (UnQual (HsIdent "fileDescriptorProto")) []) (HsUnGuardedRhs $
                    lvar "getFromBS" $$
                      HsParen (lvar "wireGet" $$ litInt 11) $$ 
                      HsParen (lvar "pack" $$ HsLit (HsString (LC.unpack bs)))) noWhere

descriptorModule :: DescriptorInfo -> HsModule
descriptorModule di
    = let protoName = descName di
          prefix = haskellPrefix protoName
          un = UnQual . HsIdent . baseName $ protoName
          imports = standardImports (hasExt di) ++ map formatImport' (toImport' di)
          exportKeys = map (HsEVar . UnQual . HsIdent . baseName . fieldName . snd) (F.toList (keys di))
          formatImport' ((a,b),s) = HsImportDecl src (Module a) True (Just (Module b)) (Just (False,
                                      map (HsIAbs . HsIdent) (S.toList s)))
          formatImport (m,t) = HsImportDecl src (Module (dotPre prefix (dotPre m t))) True
                                 (Just (Module m)) (Just (False,[HsIAbs (HsIdent t)]))
      in HsModule src (Module (fqName protoName))
           (Just (HsEThingAll un : exportKeys))
           imports (descriptorX di : (keysX protoName (keys di) ++ instancesDescriptor di))

standardImports :: Bool -> [HsImportDecl]
standardImports ext =
  [ HsImportDecl src (Module "Prelude") False Nothing (Just (False,ops))
  , HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]
 where ops | ext = map (HsIVar . HsSymbol) ["+","<=","&&"," || "]
           | otherwise = map (HsIVar . HsSymbol) ["+"]

toImport' :: DescriptorInfo -> [((String,String),S.Set String)]
toImport' di
    = M.assocs . M.fromListWith S.union . filter isForeign . map withMod $ allNames
  where isForeign = let here = fqName protoName
                    in (\((a,_),_) -> a/=here)
        protoName@(ProtoName _prefix parents self) = descName di
        withMod p@(ProtoName prefix modname base) | isUpper (head base) = ((fqName p,modname),S.singleton base)
                                                  | otherwise = ((dotPre prefix modname,modname),S.singleton base)
        allNames = F.foldr addName keyNames (fields di)
        keyNames = F.foldr (\(e,fi) rest -> e : addName fi rest) keysKnown (keys di)
        keysKnown = F.foldr (\fi rest -> fieldName fi : rest) [] (knownKeys di)
--      keysKnown = F.foldr (\fi rest -> addName fi (fieldName fi : rest)) [] (knownKeys di)
        addName fi rest = maybe rest (:rest) (typeName fi)

toImport :: DescriptorInfo -> [(String,String)]
toImport di
    = map head . group . sort 
      . filter (selfName /=)
      . concatMap withMod
      $ allNames
  where selfName = (parentModule (descName di), baseName (descName di))
        withMod (ProtoName _prefix "" _base) = []
        withMod (ProtoName _prefix modname base) = [(modname,base)]
        allNames = F.foldr addName keyNames (fields di)
        keyNames = F.foldr (\(e,fi) rest -> e : addName fi rest) keysKnown (keys di)
        keysKnown = F.foldr addName [] (knownKeys di)
        addName fi rest = maybe rest (:rest) (typeName fi)

keysX :: ProtoName -> Seq KeyInfo -> [HsDecl]
keysX self i = concatMap (makeKey self) . F.toList $ i

makeKey :: ProtoName -> KeyInfo -> [HsDecl]
makeKey self (extendee,f) = [ keyType, keyVal ]
  where keyType = HsTypeSig src [ HsIdent (baseName . fieldName $ f) ] (HsQualType [] (foldl1 HsTyApp . map HsTyCon $
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
        keyVal = HsPatBind src (HsPApp (UnQual (HsIdent (baseName (fieldName f)))) []) (HsUnGuardedRhs
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
       
descriptorX :: DescriptorInfo -> HsDecl
descriptorX di = HsDataDecl src [] name [] [con] derives
  where self = descName di
        name = HsIdent (baseName self)
        con = HsRecDecl src name eFields
                where eFields = F.foldr ((:) . fieldX) end (fields di)
                      end = if hasExt di then [extfield] else []
        extfield :: ([HsName],HsBangType)
        extfield = ([HsIdent "ext'field"],HsUnBangedTy (HsTyCon (Qual (Module "P'") (HsIdent "ExtField"))))

        fieldX :: FieldInfo -> ([HsName],HsBangType)
        fieldX fi = ([HsIdent (baseName $ fieldName fi)],HsUnBangedTy (labeled (HsTyCon typed)))
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
   [ instanceMergeable
   , instanceDefault
   , instanceWireDescriptor
   , instanceMessageAPI . descName
   , instanceGPB . descName                 
   , instanceReflectDescriptor
   ]

instanceExtendMessage :: DescriptorInfo -> HsDecl
instanceExtendMessage di
    = HsInstDecl src [] (private "ExtendMessage") [HsTyCon (UnQual (HsIdent (baseName (descName di))))]
        [ inst "getExtField" [] (lvar "ext'field")
        , inst "putExtField" [HsPVar (HsIdent "e'f"),HsPVar (HsIdent "msg")] putextfield
        , inst "validExtRanges" [ HsPVar (HsIdent "msg") ] (pvar "extRanges" $$ (HsParen $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (UnQual (HsIdent "ext'field")) (lvar "e'f") ]

instanceMergeable :: DescriptorInfo -> HsDecl
instanceMergeable di
    = HsInstDecl src [] (private "Mergeable") [HsTyCon un]
        [ inst "mergeEmpty" [] (foldl' HsApp (HsCon un) (replicate len (HsCon (private "mergeEmpty"))))
        , inst "mergeAppend" [HsPApp un patternVars1, HsPApp un patternVars2]
                             (foldl' HsApp (HsCon un) (zipWith append vars1 vars2))
        ]
  where un = UnQual (HsIdent (baseName (descName di)))
        len = (if hasExt di then succ else id) $ Seq.length (fields di)
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
  where un = UnQual (HsIdent (baseName (descName di)))
        deflistExt = F.foldr ((:) . defX) end (fields di)
        end = if hasExt di then [pvar "defaultValue"] else []

        defX :: FieldInfo -> HsExp
        defX fi | isRequired fi || canRepeat fi = dv
                | otherwise = HsParen $ HsCon (private "Just") $$ dv
          where dv = maybe (pvar "defaultValue") (defToSyntax (typeCode fi)) (hsDefault fi)

instanceMessageAPI :: ProtoName -> HsDecl
instanceMessageAPI protoName
    = HsInstDecl src [] (private "MessageAPI") [HsTyVar (HsIdent "msg'"), HsTyFun (HsTyVar (HsIdent "msg'")) (HsTyCon un),  (HsTyCon un)]
        [ inst "getVal" [HsPVar (HsIdent "m'"),HsPVar (HsIdent "f'")] (HsApp (lvar "f'" ) (lvar "m'")) ]
  where un = UnQual (HsIdent (baseName protoName))

mkOp :: String -> HsExp -> HsExp -> HsExp
mkOp s a b = HsInfixApp a (HsQVarOp (UnQual (HsSymbol s))) b

{-
        isAllowed x = pvar "or" $$ HsList ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&"; -- (||!) = mkOp "||"
          ranges = map (\(FieldId lo,FieldId hi) -> (litInt lo <=! x) &&! (x <=! litInt hi)) allowedExts
-}
instanceWireDescriptor :: DescriptorInfo -> HsDecl
instanceWireDescriptor (DescriptorInfo { descName = protoName
                                       , isGroup = isGroup
                                       , fields = fieldInfos
                                       , extRanges = allowedExts
                                       , knownKeys = fieldExts })
  = let me = unqualName protoName
        extensible = not (null allowedExts)
        len = (if extensible then succ else id) $ Seq.length fieldInfos
        mine = HsPApp me . take len . map (\n -> HsPVar (HsIdent ("x'" ++ show n))) $ [1..]
        vars = take len . map (\n -> lvar ("x'" ++ show n)) $ [1..]

        cases g m = HsCase (lvar "ft'") [ HsAlt src (litIntP 10) (HsUnGuardedAlt g) noWhere
                                        , HsAlt src (litIntP 11) (HsUnGuardedAlt m) noWhere]

        sizeCases = HsUnGuardedRhs (cases (lvar "calc'Size") (lvar "calc'Size"))
        whereCalcSize = [HsFunBind [HsMatch src (HsIdent "calc'Size") [] (HsUnGuardedRhs sizes) noWhere]]
        sizes | null sizesListExt = HsLit (HsInt 0)
              | otherwise = HsParen (foldl1' (+!) sizesListExt)
          where (+!) = mkOp "+"
                sizesListExt | extensible = sizesList ++ [ pvar "wireSizeExtField" $$ last vars ]
                             | otherwise = sizesList
                sizesList =  zipWith toSize vars . F.toList $ fieldInfos
        toSize var fi = let f = if isRequired fi then "wireSizeReq"
                                  else if canRepeat fi then "wireSizeRep"
                                      else "wireSizeOpt"
                        in foldl' HsApp (pvar f) [ litInt (wireTagLength fi)
                                                 , litInt (getFieldType (typeCode fi))
                                                 , var]

        putCases = HsUnGuardedRhs (cases (lvar "put'Fields") (HsDo 
                    [ HsQualifier $ pvar "putSize" $$
                        (HsParen $ foldl' HsApp (pvar "wireSize") [ litInt 11 , lvar "self'" ])
                    , HsQualifier $ lvar "put'Fields" ]))
        wherePutFields = [HsFunBind [HsMatch src (HsIdent "put'Fields") [] (HsUnGuardedRhs (HsDo putStmts)) noWhere]]
        putStmts = putStmtsContent
          where putStmtsContent | null putStmtsListExt = [HsQualifier $ pvar "return" $$ HsCon (Special HsUnitCon)]
                                | otherwise = putStmtsListExt
                putStmtsListExt | extensible = putStmtsList ++ [ HsQualifier $ pvar "wirePutExtField" $$ last vars ]
                                | otherwise = putStmtsList
                putStmtsList = zipWith toPut vars . F.toList $ fieldInfos
        toPut var fi = let f = if isRequired fi then "wirePutReq"
                                 else if canRepeat fi then "wirePutRep"
                                     else "wirePutOpt"
                       in HsQualifier $
                          foldl' HsApp (pvar f) [ litInt (getWireTag (wireTag fi))
                                                , litInt (getFieldType (typeCode fi))
                                                , var]

        getCases = HsUnGuardedRhs (cases (pvar (if extensible then "getBareMessageExt" else "getBareMessage") $$ lvar "update'Self")
                                         (pvar (if extensible then "getMessageExt" else "getMessage") $$ lvar "update'Self"))
        whereUpdateSelf = [HsFunBind [HsMatch src (HsIdent "update'Self")
                            [HsPVar (HsIdent "field'Number") ,HsPVar (HsIdent "old'Self")]
                            (HsUnGuardedRhs (HsCase (lvar "field'Number") updateAlts)) noWhere]]
        updateAlts = map toUpdate (F.toList fieldInfos) 
                     ++ (if extensible && (not (Seq.null fieldExts)) then map toUpdateExt (F.toList fieldExts) else [])
                     ++ [HsAlt src HsPWildCard (HsUnGuardedAlt $
                           pvar "unknownField" $$ (lvar "field'Number")) noWhere]
        toUpdateExt fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $
                           pvar "wireGetKey" $$ HsVar (mayQualName protoName (fieldName fi)) $$ lvar "old'Self") noWhere
        -- fieldIds cannot be negative so no parenthesis are required to protect a negative sign
        toUpdate fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $ 
                        pvar "fmap" $$ (HsParen $ HsLambda src [HsPVar (HsIdent "new'Field")] $
                                          HsRecUpdate (lvar "old'Self") [HsFieldUpdate (UnQual . HsIdent . baseName . fieldName $ fi)
                                                                                       (labelUpdate fi)])
                                    $$ (HsParen (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ HsParen ((lvar . baseName . fieldName $ fi) $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (HsCon (private "Just") $$ lvar "new'Field")
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,11] =
                               pvar "mergeAppend" $$ HsParen ((lvar . baseName . fieldName $ fi) $$ lvar "old'Self") $$ (HsParen x)
                           | otherwise = x
         
    in HsInstDecl src [] (private "Wire") [HsTyCon me]
        [ HsFunBind [HsMatch src (HsIdent "wireSize") [HsPVar (HsIdent "ft'"),(HsPParen mine)] sizeCases whereCalcSize]
        , HsFunBind [HsMatch src (HsIdent "wirePut") [HsPVar (HsIdent "ft'"),HsPAsPat (HsIdent "self'") (HsPParen mine)] putCases wherePutFields]
        , HsFunBind [HsMatch src (HsIdent "wireGet") [HsPVar (HsIdent "ft'")] getCases whereUpdateSelf]
        ]

instanceReflectDescriptor :: DescriptorInfo -> HsDecl
instanceReflectDescriptor di
    = HsInstDecl src [] (private "ReflectDescriptor") [HsTyCon (UnQual (HsIdent (baseName (descName di))))]
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

-----------------------
{-
test = putStrLn . prettyPrint . descriptorModule False "Text" $ d'

testDesc =  putStrLn . prettyPrint . descriptorModule False "Text" $ genFieldOptions

testLabel = putStrLn . prettyPrint $ enumModule "Text" labelTest
testType = putStrLn . prettyPrint $ enumModule "Text" t

-- testing
utf8FromString = Utf8 . U.fromString

-- try and generate a small replacement for my manual file
genFieldOptions :: D.DescriptorProto.DescriptorProto
genFieldOptions =
  defaultValue
  { D.DescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldOptions") 
  , D.DescriptorProto.field = Seq.fromList
    [ defaultValue
      { D.FieldDescriptorProto.name = Just (utf8FromString "ctype")
      , D.FieldDescriptorProto.number = Just 1
      , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
      , D.FieldDescriptorProto.type' = Just TYPE_ENUM
      , D.FieldDescriptorProto.type_name = Just (utf8FromString "DescriptorProtos.FieldOptions.CType")
      , D.FieldDescriptorProto.default_value = Nothing
      }
    , defaultValue
      { D.FieldDescriptorProto.name = Just (utf8FromString "experimental_map_key")
      , D.FieldDescriptorProto.number = Just 9
      , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
      , D.FieldDescriptorProto.type' = Just TYPE_STRING
      , D.FieldDescriptorProto.default_value = Nothing
      }
    ]
  }

-- test several features
d' :: D.DescriptorProto.DescriptorProto
d' = defaultValue
    { D.DescriptorProto.name = Just (utf8FromString "SomeMod.ServiceOptions") 
    , D.DescriptorProto.field = Seq.fromList
       [ defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldString")
         , D.FieldDescriptorProto.number = Just 1
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "Hello World")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldDouble")
         , D.FieldDescriptorProto.number = Just 4
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_DOUBLE
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "+5.5e-10")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldBytes")
         , D.FieldDescriptorProto.number = Just 2
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString . map toEnum $ [0,5..255])
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldInt64")
         , D.FieldDescriptorProto.number = Just 3
         , D.FieldDescriptorProto.label = Just LABEL_REQUIRED
         , D.FieldDescriptorProto.type' = Just TYPE_INT64
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "-0x40")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "fieldBool")
         , D.FieldDescriptorProto.number = Just 5
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         , D.FieldDescriptorProto.default_value = Just (utf8FromString "False")
        }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field2TestSelf")
         , D.FieldDescriptorProto.number = Just 6
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "ServiceOptions")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field3TestQualified")
         , D.FieldDescriptorProto.number = Just 7
         , D.FieldDescriptorProto.label = Just LABEL_REPEATED
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "A.B.C.Label")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (utf8FromString "field4TestUnqualified")
         , D.FieldDescriptorProto.number = Just 8
         , D.FieldDescriptorProto.label = Just LABEL_REPEATED
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (utf8FromString "Maybe")
         }
       ]
    }

labelTest :: D.EnumDescriptorProto.EnumDescriptorProto
labelTest = defaultValue
    { D.EnumDescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldDescriptorProto.Label")
    , D.EnumDescriptorProto.value = Seq.fromList
      [ defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_OPTIONAL")
                     , D.EnumValueDescriptorProto.number = Just 1 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_REQUIRED")
                     , D.EnumValueDescriptorProto.number = Just 2 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString "LABEL_REPEATED")
                     , D.EnumValueDescriptorProto.number = Just 3 }
      ]
    }

t :: D.EnumDescriptorProto.EnumDescriptorProto
t = defaultValue { D.EnumDescriptorProto.name = Just (utf8FromString "DescriptorProtos.FieldDescriptorProto.Type")
                 , D.EnumDescriptorProto.value = Seq.fromList . zipWith make [1..] $ names }
  where make :: Int32 -> String -> D.EnumValueDescriptorProto
        make i s = defaultValue { D.EnumValueDescriptorProto.name = Just (utf8FromString s)
                                , D.EnumValueDescriptorProto.number = Just i }
        names = ["TYPE_DOUBLE","TYPE_FLOAT","TYPE_INT64","TYPE_UINT64","TYPE_INT32"
                ,"TYPE_FIXED64","TYPE_FIXED32","TYPE_BOOL","TYPE_STRING","TYPE_GROUP"
                ,"TYPE_MESSAGE","TYPE_BYTES","TYPE_UINT32","TYPE_ENUM","TYPE_SFIXED32"
                ,"TYPE_SFIXED64","TYPE_SINT32","TYPE_SINT64"]
-}