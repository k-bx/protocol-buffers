-- This module uses the Reflection data structures (ProtoInfo,EnumInfo,DescriptorInfo) to
-- build an AST using Language.Haskell.Syntax.  This get quite verbose, so a large number
-- of helper functions (and operators) are defined to aid in specifying the output code.
--
-- Note that this may eventually also generate hs-boot files to allow
-- for breaking mutual recursion.
--
-- Mangling: For the current moment, assume the mangling is done in a prior pass:
--   (*) Uppercase all module names and type names and enum constants
--   (*) lowercase all field names
--   (*) add a prime after all field names than conflict with reserved words
--
-- The names are also assumed to have become fully-qualified, and all
-- the optional type codes have been set.
--
module Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModule,enumModule,prettyPrint) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections(KeyInfo,HsDefault(..),DescriptorInfo(..),ProtoInfo(..),EnumInfo(..),ProtoName(..),ProtoFName(..),FieldInfo(..))

import qualified Data.ByteString.Lazy.Char8 as LC(unpack)
import qualified Data.Foldable as F(foldr,toList)
import Data.List(sortBy,foldl',foldl1',sort)
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

litStr :: String -> HsExp
litStr = HsLit . HsString

litIntP :: Integral x => x -> HsPat
litIntP x | x<0 = HsPParen $ HsPLit (HsInt (toInteger x))
          | otherwise = HsPLit (HsInt (toInteger x))

litInt :: Integral x => x -> HsExp
litInt x | x<0 = HsParen $ HsLit (HsInt (toInteger x))
         | otherwise = HsLit (HsInt (toInteger x))

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (private s))

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)

local :: String -> HsQName
local t = UnQual (HsIdent t)

pvar :: String -> HsExp
pvar t = HsVar (private t)

pcon :: String -> HsExp
pcon t = HsCon (private t)

lvar :: String -> HsExp
lvar t = HsVar (local t)

lcon :: String -> HsExp
lcon t = HsCon (local t)

var :: String -> HsPat
var t = HsPVar (HsIdent t)

match :: String -> [HsPat] -> HsExp -> HsMatch
match s p r = HsMatch src (HsIdent s) p (HsUnGuardedRhs r) noWhere

inst :: String -> [HsPat] -> HsExp -> HsDecl
inst s p r  = HsFunBind [match s p r]

mkOp :: String -> HsExp -> HsExp -> HsExp
mkOp s a b = HsInfixApp a (HsQVarOp (UnQual (HsSymbol s))) b

compose :: HsExp -> HsExp -> HsExp
compose = mkOp "."

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
enumModule :: EnumInfo -> HsModule
enumModule ei
    = let protoName = enumName ei
      in HsModule src (Module (fqMod protoName))
           (Just [HsEThingAll (UnQual (baseIdent protoName))])
           (standardImports True False) (enumDecls ei)

enumDecls :: EnumInfo -> [HsDecl]
enumDecls ei =  map ($ ei) [ enumX
                           , instanceMergeableEnum
                           , instanceBounded
                           , instanceDefaultEnum ]
                ++ declToEnum ei ++
                map ($ ei) [ instanceEnum
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
        set f (_,n) = inst f [] (lcon n)

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
                       (:) (_,n) _ -> lcon n
                       [] -> error $ "Impossible? EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show ei

declToEnum :: EnumInfo -> [HsDecl]
declToEnum ei = [ HsTypeSig src [HsIdent "toMaybe'Enum"]
                    (HsQualType [] (HsTyFun (HsTyCon (private "Int"))
                                            (typeApp "Maybe" (HsTyCon (unqualName (enumName ei))))))
                , HsFunBind (map toEnum'one values ++ [final]) ]
  where values = enumValues ei
        toEnum'one (v,n) = match "toMaybe'Enum" [litIntP (getEnumCode v)] (pcon "Just" $$ lcon n)
        final = match "toMaybe'Enum" [HsPWildCard] (pcon "Nothing")

instanceEnum :: EnumInfo -> HsDecl
instanceEnum ei
    = HsInstDecl src [] (private "Enum") [HsTyCon (unqualName (enumName ei))]
        (map HsFunBind [fromEnum',toEnum',succ',pred'])
  where values = enumValues ei
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = match "fromEnum" [HsPApp (local n) []] (litInt (getEnumCode v))
        toEnum' = [ match "toEnum" [] (compose mayErr (lvar "toMaybe'Enum")) ]
        mayErr = pvar "fromMaybe" $$ (HsParen (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: toEnum failure for type "++ fqMod (enumName ei))))
        succ' = zipWith (equate "succ") values (tail values) ++
                [ match "succ" [HsPWildCard] (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: succ failure for type "++ fqMod (enumName ei))) ]
        pred' = zipWith (equate "pred") (tail values) values ++
                [ match "pred" [HsPWildCard] (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: pred failure for type "++ fqMod (enumName ei))) ]
        equate f (_,n1) (_,n2) = match f [HsPApp (local n1) []] (lcon n2)

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: EnumInfo -> HsDecl
instanceWireEnum ei
    = HsInstDecl src [] (private "Wire") [HsTyCon (unqualName (enumName ei))]
        [ withName "wireSize", withName "wirePut", withGet, withGetErr ]
  where withName foo = inst foo [var "ft'",var "enum"] rhs
          where rhs = pvar foo $$ lvar "ft'" $$
                        (HsParen $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [litIntP 14] rhs
          where rhs = pvar "wireGetEnum" $$ lvar "toMaybe'Enum"
        withGetErr = inst "wireGet" [var "ft'"] rhs
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
        xxx'Exp = HsParen $ pvar "pack" $$ litStr (LC.unpack (utf8 (fiName xxx)))
        values = enumValues ei
        ascList,ei',protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,ns) = HsTuple [litInt (getEnumCode v),litStr ns,lcon ns]
        ei' = foldl' HsApp (pcon "EnumInfo") [protoNameExp
                                             ,HsList $ map litStr (enumFilePath ei)
                                             ,HsList (map two values)]
          where two (v,ns) = HsTuple [litInt (getEnumCode v),litStr ns]
        protoNameExp = HsParen $ foldl' HsApp (pvar "makePNF")
                                        [ xxx'Exp, mList a, mList b, litStr (mName c) ]
          where mList = HsList . map (litStr . mName)

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

--------------------------------------------
-- FileDescriptorProto module creation
--------------------------------------------

protoModule :: ProtoInfo -> ByteString -> HsModule
protoModule pri@(ProtoInfo protoName _ _ keyInfos _ _ _) fdpBS
  = let exportKeys = map (HsEVar . UnQual . baseIdent' . fieldName . snd) (F.toList keyInfos)
        exportNames = map (HsEVar . UnQual . HsIdent) ["protoInfo","fileDescriptorProto"]
        imports = protoImports ++ map formatImport (protoImport pri)
    in HsModule src (Module (fqMod protoName)) (Just (exportKeys++exportNames)) imports (keysX protoName keyInfos ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
  where protoImports = standardImports False (not . Seq.null . extensionKeys $ pri) ++
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
  where myType = HsTypeSig src [ HsIdent "protoInfo" ] (HsQualType [] (HsTyCon (local "ProtoInfo")))
        myValue = HsPatBind src (HsPApp (local "protoInfo") []) (HsUnGuardedRhs $
                    pvar "read" $$ litStr (show pri)) noWhere

embed'fdpBS :: ByteString -> [HsDecl]
embed'fdpBS bs = [ myType, myValue ]
  where myType = HsTypeSig src [ HsIdent "fileDescriptorProto" ] (HsQualType [] (HsTyCon (local "FileDescriptorProto")))
        myValue = HsPatBind src (HsPApp (local "fileDescriptorProto") []) (HsUnGuardedRhs $
                    pvar "getFromBS" $$
                      HsParen (pvar "wireGet" $$ litInt 11) $$ 
                      HsParen (pvar "pack" $$ litStr (LC.unpack bs))) noWhere

--------------------------------------------
-- DescriptorProto module creation
--------------------------------------------
descriptorModule :: DescriptorInfo -> HsModule
descriptorModule di
    = let protoName = descName di
          un = UnQual . baseIdent $ protoName
          imports = standardImports False (hasExt di) ++ map formatImport (toImport di)
          exportKeys = map (HsEVar . UnQual . baseIdent' . fieldName . snd) (F.toList (keys di))
          formatImport ((a,b),s) = HsImportDecl src (Module a) True asM (Just (False, map (HsIAbs . HsIdent) (S.toList s)))
            where asM | a==b = Nothing
                      | otherwise = Just (Module b)
      in HsModule src (Module (fqMod protoName))
           (Just (HsEThingAll un : exportKeys))
           imports (descriptorX di : (keysX protoName (keys di) ++ instancesDescriptor di))

standardImports :: Bool -> Bool -> [HsImportDecl]
standardImports en ext =
  [ HsImportDecl src (Module "Prelude") False Nothing (Just (False,ops))
  , HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]
 where ops | ext = map (HsIVar . HsSymbol) $ base ++ ["==","<=","&&"," || "]
           | otherwise = map (HsIVar . HsSymbol) base
       base | en = ["+","."]
            | otherwise = ["+"]

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
                               $$ maybe (pvar "Nothing")
                                        (HsParen . (pvar "Just" $$) . (defToSyntax (typeCode f)))
                                        (hsDefault f)
                   )) noWhere

defToSyntax :: FieldType -> HsDefault -> HsExp
defToSyntax tc x =
  case x of
    HsDef'Bool b -> pcon (show b)
    HsDef'ByteString bs -> (if tc == 9 then (\xx -> HsParen (pvar "Utf8" $$ xx)) else id) $
                           (HsParen $ pvar "pack" $$ litStr (LC.unpack bs))
    HsDef'Rational r | r < 0 -> HsParen $ HsLit (HsFrac r)
                     | otherwise -> HsLit (HsFrac r)
    HsDef'Integer i -> litInt i 
    HsDef'Enum s -> HsParen $ pvar "read" $$ litStr s

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
        , inst "putExtField" [var "e'f", var "msg"] putextfield
        , inst "validExtRanges" [var "msg"] (pvar "extRanges" $$ (HsParen $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (local "ext'field") (lvar "e'f") ]

instanceUnknownMessage :: DescriptorInfo -> HsDecl
instanceUnknownMessage di
    = HsInstDecl src [] (private "UnknownMessage") [HsTyCon (UnQual (baseIdent (descName di)))]
        [ inst "getUnknownField" [] (lvar "unknown'field")
        , inst "putUnknownField" [var "u'f",var "msg"] putunknownfield
        ]
  where putunknownfield = HsRecUpdate (lvar "msg") [ HsFieldUpdate (local "unknown'field") (lvar "u'f") ]

instanceMergeable :: DescriptorInfo -> HsDecl
instanceMergeable di
    = HsInstDecl src [] (private "Mergeable") [HsTyCon un]
        [ inst "mergeEmpty" [] (foldl' HsApp (HsCon un) (replicate len (pcon "mergeEmpty")))
        , inst "mergeAppend" [HsPApp un patternVars1, HsPApp un patternVars2]
                             (foldl' HsApp (HsCon un) (zipWith append vars1 vars2))
        ]
  where un = UnQual (baseIdent (descName di))
        len = (if hasExt di then succ else id)
            $ (if storeUnknown di then succ else id)
            $ Seq.length (fields di)
        patternVars1,patternVars2 :: [HsPat]
        patternVars1 = take len inf
            where inf = map (\n -> var ("x'" ++ show n)) [1..]
        patternVars2 = take len inf
            where inf = map (\n -> var ("y'" ++ show n)) [1..]
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
                        Just hsdef -> HsParen $ pcon "Just" $$ defToSyntax (typeCode fi) hsdef

instanceMessageAPI :: ProtoName -> HsDecl
instanceMessageAPI protoName
    = HsInstDecl src [] (private "MessageAPI")
        [HsTyVar (HsIdent "msg'"), HsTyFun (HsTyVar (HsIdent "msg'")) (HsTyCon un),  (HsTyCon un)]
        [ inst "getVal" [var "m'",var "f'"] (HsApp (lvar "f'" ) (lvar "m'")) ]
  where un = UnQual (baseIdent protoName)

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
        mine = HsPApp me . take len . map (\n -> var ("x'" ++ show n)) $ [1..]
        vars = take len . map (\n -> lvar ("x'" ++ show n)) $ [1..]
        mExt | extensible = Just (vars !! Seq.length fieldInfos)
             | otherwise = Nothing
        mUnknown | storeUnknown di = Just (last vars)
                 | otherwise = Nothing

        -- first case is for Group behavior, second case is for Message behavior, last is error handler
        cases g m e = HsCase (lvar "ft'") [ HsAlt src (litIntP 10) (HsUnGuardedAlt g) noWhere
                                          , HsAlt src (litIntP 11) (HsUnGuardedAlt m) noWhere
                                          , HsAlt src HsPWildCard  (HsUnGuardedAlt e) noWhere
                                          ]

        sizeCases = HsUnGuardedRhs $ cases (lvar "calc'Size") 
                                           (pvar "prependMessageSize" $$ lvar "calc'Size")
                                           (pvar "wireSizeErr" $$ lvar "ft'" $$ lvar "self'")
        whereCalcSize = [inst "calc'Size" [] sizes]
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
        wherePutFields = [inst "put'Fields" [] (HsDo putStmts)]
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
          (pvar "getBareMessageWith" $$ lvar "check'allowed")
          (pvar "getMessageWith" $$ lvar "check'allowed")
          (pvar "wireGetErr" $$ lvar "ft'")
        whereDecls = [whereUpdateSelf,whereAllowed,whereCheckAllowed]
        whereAllowed = inst "allowed'wire'Tags" [] (pvar "fromDistinctAscList" $$ HsList (map litInt allowed))
        allowed = sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di)] ++
                         [ getWireTag (wireTag f) | f <- F.toList (knownKeys di)]
        locals = ["wire'Tag","field'Number","wire'Type","old'Self"]
        whereCheckAllowed = inst "check'allowed" (map var locals) process
         where process = if storeUnknown di then catchUn updateBranch else updateBranch
               catchUn s = pvar "catchError" $$ HsParen s
                 $$ HsParen (HsLambda src [HsPWildCard] (args (pvar "loadUnknown")))
               updateBranch | null allowed = extBranch
                            | otherwise = HsIf (pvar "member" $$ lvar "wire'Tag" $$ lvar "allowed'wire'Tags")
                                               (lvar "update'Self" $$ lvar "field'Number" $$ lvar "old'Self")
                                               extBranch
               extBranch | extensible = HsIf (isAllowedExt (lvar "field'Number"))
                                             (args (pvar "loadExtension"))
                                             unknownBranch
                         | otherwise = unknownBranch
               unknownBranch =args (pvar "unknown")
               args x = x $$ lvar "field'Number" $$ lvar "wire'Type" $$ lvar "old'Self"
        isAllowedExt x = pvar "or" $$ HsList ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&"; (==!) = mkOp ("==")
          ranges = map (\(FieldId lo,FieldId hi) -> if hi < maxHi
                                                      then if lo == hi
                                                             then (x ==! litInt lo)
                                                             else (litInt lo <=! x) &&! (x <=! litInt hi)
                                                      else litInt lo <=! x) allowedExts
             where FieldId maxHi = maxBound
        whereUpdateSelf = inst "update'Self" [var "field'Number", var "old'Self"]
                            (HsCase (lvar "field'Number") updateAlts)
        updateAlts = map toUpdate (F.toList fieldInfos)
                     ++ (if extensible && (not (Seq.null fieldExts)) then map toUpdateExt (F.toList fieldExts) else [])
                     ++ [HsAlt src HsPWildCard (HsUnGuardedAlt $
                           pvar "unknownField" $$ (lvar "old'Self") $$ (lvar "field'Number")) noWhere]
        toUpdateExt fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $
                           pvar "wireGetKey" $$ HsVar (mayQualName protoName (fieldName fi)) $$ lvar "old'Self") noWhere
        toUpdate fi = HsAlt src (litIntP . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $ 
                        pvar "fmap" $$ (HsParen $ HsLambda src [var "new'Field"] $
                                          HsRecUpdate (lvar "old'Self")
                                                      [HsFieldUpdate (UnQual . baseIdent' . fieldName $ fi)
                                                                     (labelUpdate fi)])
                                    $$ (HsParen (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ HsParen ((lvar . fName . baseName' . fieldName $ fi)
                                                                  $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (pcon "Just" $$ lvar "new'Field")
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,11] =
                               pvar "mergeAppend" $$ HsParen ( (lvar . fName . baseName' . fieldName $ fi)
                                                               $$ lvar "old'Self" )
                                                  $$ HsParen x
                           | otherwise = x
        -- in the above, the [10,11] check optimizes using the
        -- knowledge that only TYPE_MESSAGE and TYPE_GROUP have merges
        -- that are not right-biased replacements.  The "append" uses
        -- knowledge of how all repeated fields get merged.
    in HsInstDecl src [] (private "Wire") [HsTyCon me]
        [ HsFunBind [HsMatch src (HsIdent "wireSize") [var "ft'",HsPAsPat (HsIdent "self'") (HsPParen mine)] sizeCases whereCalcSize]
        , HsFunBind [HsMatch src (HsIdent "wirePut")  [var "ft'",HsPAsPat (HsIdent "self'") (HsPParen mine)] putCases wherePutFields]
        , HsFunBind [HsMatch src (HsIdent "wireGet") [var "ft'"] getCases whereDecls]
        ]

instanceReflectDescriptor :: DescriptorInfo -> HsDecl
instanceReflectDescriptor di
    = HsInstDecl src [] (private "ReflectDescriptor") [HsTyCon (UnQual (baseIdent (descName di)))]
        [ inst "getMessageInfo" [HsPWildCard] gmi
        , inst "reflectDescriptorInfo" [ HsPWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: HsExp
        rdi = pvar "read" $$ litStr (show di) -- cheat using show and read
        gmi,reqId,allId :: HsExp
        gmi = pcon "GetMessageInfo" $$ HsParen reqId $$ HsParen allId
        reqId = pvar "fromDistinctAscList" $$
                HsList (map litInt . sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di), isRequired f])
        allId = pvar "fromDistinctAscList" $$
                HsList (map litInt . sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di)] ++
                                            [ getWireTag (wireTag f) | f <- F.toList (knownKeys di)])

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
