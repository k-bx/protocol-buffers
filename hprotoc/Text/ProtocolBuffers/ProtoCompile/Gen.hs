{-# LANGUAGE NamedFieldPuns, RecordWildCards, CPP #-}
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
module Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModules,enumModule,oneofModule,prettyPrint) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections(KeyInfo,HsDefault(..),SomeRealFloat(..),DescriptorInfo(..),ProtoInfo(..),OneofInfo(..),EnumInfo(..),ProtoName(..),ProtoFName(..),FieldInfo(..))

import Text.ProtocolBuffers.ProtoCompile.BreakRecursion(Result(..),VertexKind(..),pKey,pfKey,getKind,Part(..))

--import Paths_hprotoc(version)
--import Data.Version(showVersion)
import qualified Data.ByteString.Lazy.Char8 as LC(unpack)
import qualified Data.Foldable as F(foldr,toList)
import Data.List(sortBy,foldl',foldl1',group,sort,union)
import Data.Function(on)
import Language.Haskell.Exts.Pretty(prettyPrint)
import Language.Haskell.Exts.Syntax hiding (Int,String)
import Language.Haskell.Exts.SrcLoc
import Language.Haskell.Exts.Syntax as Hse
import Data.Char(isLower,isUpper)
import qualified Data.Map as M
import Data.Maybe(mapMaybe)
import           Data.Sequence (ViewL(..),(><))
import qualified Data.Sequence as Seq(null,length,empty,viewl)
import qualified Data.Set as S
import System.FilePath(joinPath)

ecart :: String -> a -> a
ecart _ x = x

default (Int)

-- -- -- -- Helper functions

imp :: String -> a
imp s = error ("Impossible? Text.ProtocolBuffers.ProtoCompile.Gen."++s)

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

#if MIN_VERSION_haskell_src_exts(1, 17, 0)
noWhere :: Maybe Binds
noWhere = Nothing
#else
noWhere :: Binds
noWhere = BDecls []
#endif

#if MIN_VERSION_haskell_src_exts(1, 17, 0)
whereBinds :: Binds -> Maybe Binds
whereBinds = Just
#else
whereBinds :: Binds -> Binds
whereBinds = id
#endif


($$) :: Exp -> Exp -> Exp
($$) = App

infixl 1 $$

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

litStr :: String -> Exp
litStr = Lit . Hse.String

litIntP :: Integral x => x -> Pat
litIntP x | x<0 = PParen $ PLit Signless (Hse.Int (toInteger x))
          | otherwise = PLit Signless (Hse.Int (toInteger x))

-- Pin down the type inference
litIntP' :: Int -> Pat
litIntP' = litIntP

litInt :: Integral x => x -> Exp
litInt x | x<0 = Paren $ Lit (Hse.Int (toInteger x))
         | otherwise = Lit (Hse.Int (toInteger x))

litInt' :: Int -> Exp
litInt' = litInt

typeApp :: String -> Type -> Type
typeApp s =  TyApp (TyCon (private s))

-- private is for Text.ProtocolBuffers.Header, prelude is for Prelude, local is unqualified
private :: String -> QName
private t = Qual (ModuleName "P'") (Ident t)

prelude :: String -> QName
prelude t = Qual (ModuleName "Prelude'") (Ident t)

local :: String -> QName
local t = UnQual (Ident t)

localField :: DescriptorInfo -> String -> QName
localField di t = UnQual (fieldIdent di t)

-- pvar and preludevar and lvar are for lower-case identifiers
isVar :: String -> Bool
isVar (x:_) = isLower x || x == '_'
isVar _ = False

isCon :: String -> Bool
isCon (x:_) = isUpper x
isCon _ = False

pvar :: String -> Exp
pvar t | isVar t = Var (private t)
       | otherwise = error $ "hprotoc Gen.hs assertion failed: pvar expected lower-case first letter in " ++ show t

preludevar :: String -> Exp
preludevar t | isVar t = Var (prelude t)
             | otherwise = error $ "hprotoc Gen.hs assertion failed: preludevar expected lower-case first letter in " ++ show t

lvar :: String -> Exp
lvar t | isVar t = Var (local t)
       | otherwise = error $ "hprotoc Gen.hs assertion failed: lvar expected lower-case first letter in " ++ show t

-- pcon and preludecon and lcon are for upper-case identifiers
pcon :: String -> Exp
pcon t | isCon t = Con (private t)
       | otherwise = error $ "hprotoc Gen.hs assertion failed: pcon expected upper-case first letter in " ++ show t

preludecon :: String -> Exp
preludecon t | isCon t = Con (prelude t)
             | otherwise = error $ "hprotoc Gen.hs assertion failed: preludecon expected upper-case first letter in " ++ show t

lcon :: String -> Exp
lcon t | isCon t = Con (local t)
       | otherwise = error $ "hprotoc Gen.hs assertion failed: lcon expected upper-case first letter in " ++ show t

-- patvar is a pattern that binds a new lower-case variable name
patvar :: String -> Pat
patvar t | isVar t = PVar (Ident t)
         | otherwise = error $ "hprotoc Gen.hs assertion failed: patvar expected lower-case first letter in " ++ show t

match :: String -> [Pat] -> Exp -> Match
match s p r = Match src (Ident s) p Nothing (UnGuardedRhs r) noWhere

inst :: String -> [Pat] -> Exp -> InstDecl
inst s p r  = InsDecl $ FunBind [match s p r]

defun :: String -> [Pat] -> Exp -> Decl
defun s p r  = FunBind [match s p r]

mkOp :: String -> Exp -> Exp -> Exp
mkOp s a b = InfixApp a (QVarOp (UnQual (Symbol s))) b

compose :: Exp -> Exp -> Exp
compose = mkOp "."

fqMod :: ProtoName -> String
fqMod (ProtoName _ a b c) = joinMod $ a++b++[c]

-- importPN takes the Result to look up the target info, it takes the
-- current MKey (pKey of protoName, no 'Key appended), and Part to
-- identify the module being created.  The ProtoName is the target
-- TYPE that is needed.
importPN :: Result -> ModuleName -> Part -> ProtoName -> Maybe ImportDecl
importPN r selfMod@(ModuleName self) part pn =
  let o = pKey pn
      m1 = ModuleName (joinMod (haskellPrefix pn ++ parentModule pn ++ [baseName pn]))
      m2 = ModuleName (joinMod (parentModule pn))
      fromSource = S.member (FMName self,part,o) (rIBoot r)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
      iabs = IAbs NoNamespace (Ident (mName (baseName pn)))
#else
      iabs = IAbs (Ident (mName (baseName pn)))
#endif
      ans = if m1 == selfMod && part /= KeyFile then Nothing
              else Just $ ImportDecl src m1 True fromSource False Nothing (Just m2)
                            (Just (False,[iabs]))
  in ecart (unlines . map (\ (a,b) -> a ++ " = "++b) $
                 [("selfMod",show selfMod)
                 ,("part",show part)
                 ,("pn",show pn)
                 ,("o",show o)
                 ,("m1",show m1)
                 ,("m2",show m2)
                 ,("fromSource",show fromSource)
                 ,("ans",show ans)]) $
     ans

importPFN :: Result -> ModuleName -> ProtoFName -> Maybe ImportDecl
importPFN r m@(ModuleName self) pfn =
  let o@(FMName _other) = pfKey pfn
      m1@(ModuleName m1') = ModuleName (joinMod (haskellPrefix' pfn ++ parentModule' pfn))
      m2 = ModuleName (joinMod (parentModule' pfn))
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
      spec = Just (False,[IVar (Ident (fName (baseName' pfn)))])
#else
      spec = Just (False,[IVar NoNamespace (Ident (fName (baseName' pfn)))])
#endif
      kind = getKind r o
      fromAlt = S.member (FMName self,FMName m1') (rIKey r)
      m1key = if kind == SplitKeyTypeBoot && fromAlt
                then keyFile m1
                else m1
      qualifiedFlag = (m1 /= m)
      qualifiedName | qualifiedFlag = if m2/=m1key then Just m2 else Nothing
                    | otherwise = Nothing
      sourceFlag = (kind == KeyTypeBoot) && fromAlt
      ans = if not qualifiedFlag && kind /= SplitKeyTypeBoot then Nothing else Just $
              ImportDecl src m1key qualifiedFlag sourceFlag False Nothing qualifiedName spec
  in ecart (unlines . map (\ (a,b) -> a ++ " = "++b) $
                [("m",show m)
                ,("pfn",show pfn)
                ,("o",show o)
                ,("m1",show m1)
                ,("m2",show m2)
                ,("kind",show kind)
                ,("ans",show ans)]) $
     ans


importO :: Result -> ModuleName -> Part -> OneofInfo -> Maybe [ImportDecl]
importO r selfMod@(ModuleName self) part oi =
  let pn = oneofName oi
      o = pKey pn
      m1 = ModuleName (joinMod (haskellPrefix pn ++ parentModule pn ++ [baseName pn]))
      m2 = ModuleName (joinMod (parentModule pn))
      m3 = ModuleName (joinMod (parentModule pn ++ [baseName pn]))
      fromSource = S.member (FMName self,part,o) (rIBoot r)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
      iabs1 = IAbs NoNamespace (Ident (mName (baseName pn)))
      iabsget = map (IAbs NoNamespace . Ident . fst . oneofGet) . F.toList .  oneofFields $ oi
#else
      iabs1 = IAbs (Ident (mName (baseName pn)))
      iabsget = map (IAbs . Ident . fst . oneofGet) . F.toList . oneofFields $ oi
#endif
      ithall = IThingAll (Ident (mName (baseName pn)))

      ans1 =  ImportDecl src m1 True fromSource False Nothing (Just m2)
                (Just (False,[iabs1]))
      ans2 =  ImportDecl src m1 True fromSource False Nothing (Just m3)
                (Just (False,ithall:iabsget))
  in  if m1 == selfMod && part /= KeyFile
        then Nothing
        else Just [ans1,ans2]

-- Several items might be taken from the same module, combine these statements
mergeImports :: [ImportDecl] -> [ImportDecl]
mergeImports importsIn =
  let idKey ImportDecl{..} = (importModule,importQualified,importSrc,importAs,fmap fst importSpecs)
      mergeImports' ImportDecl{importSpecs=Just(hiding,xs), ..} ImportDecl{importSpecs=Just(_,ys)} =
          ImportDecl{importSpecs=Just (hiding, xs `union` ys), ..}
      mergeImports' i _ = i -- identical, so drop one
      combined = M.fromListWith mergeImports' . map (\ i -> (idKey i,i)) $ importsIn
  in M.elems combined

keyFile :: ModuleName -> ModuleName
keyFile (ModuleName s) = ModuleName (s++"'Key")

joinMod :: [MName String] -> String
joinMod [] = ""
joinMod ms = fmName $ foldr1 dotFM . map promoteFM $ ms

baseIdent :: ProtoName -> Name
baseIdent = Ident . mName . baseName
baseIdent' :: ProtoFName -> Name
baseIdent' pfn = Ident $ baseNamePrefix' pfn ++ fName (baseName' pfn)

fieldIdent :: DescriptorInfo -> String -> Name
fieldIdent di str | makeLenses di = Ident ('_':str)
                  | otherwise = Ident str

qualName :: ProtoName -> QName
qualName p@(ProtoName _ _prefix [] _base) = UnQual (baseIdent p)
qualName p@(ProtoName _ _prefix (parents) _base) = Qual (ModuleName (joinMod parents)) (baseIdent p)

qualFName :: ProtoFName -> QName
qualFName p@(ProtoFName _ _prefix [] _base _basePrefix) = UnQual (baseIdent' p)
qualFName p@(ProtoFName _ _prefix parents _base _basePrefix) = Qual (ModuleName (joinMod parents)) (baseIdent' p)

unqualName :: ProtoName -> QName
unqualName p = UnQual (baseIdent p)

unqualFName :: ProtoFName -> QName
unqualFName p = UnQual (baseIdent' p)

mayQualName :: ProtoName -> ProtoFName -> QName
mayQualName (ProtoName _ c'prefix c'parents c'base) name@(ProtoFName _ prefix parents _base _basePrefix) =
  if joinMod (c'prefix++c'parents++[c'base]) == joinMod (prefix++parents)
    then UnQual (baseIdent' name) -- name is local, make UnQual
    else qualFName name           -- name is imported, make Qual


--------------------------------------------
-- utility for OneofInfo
--------------------------------------------

oneofCon :: (ProtoName,FieldInfo) -> Exp
oneofCon (name,_) = Con (qualName name)

oneofPat :: (ProtoName,FieldInfo) -> (Pat,Pat)
oneofPat (name,fi) =
  let fName@(Ident fname) = baseIdent' (fieldName fi)
  in (PApp (qualName name) [PVar fName],PApp (unqualName name) [PVar fName])

oneofRec :: (ProtoName,FieldInfo) -> (Exp,Exp)
oneofRec (_,fi) =
  let fName@(Ident fname) = baseIdent' (fieldName fi)
  in (litStr fname,lvar fname)

oneofGet :: (ProtoName,FieldInfo) -> (String,ProtoName)
oneofGet (p,fi) =
  let Ident fname = baseIdent' (fieldName fi)
      unqual = "get'" ++ fname
      p' = p { baseName = MName unqual }
  in (unqual,p')

--------------------------------------------
-- Define LANGUAGE options as [ModulePramga]
--------------------------------------------
modulePragmas :: Bool -> [ModulePragma]
modulePragmas templateHaskell =
  [ LanguagePragma src (map Ident $
      thPragma ++ ["BangPatterns","DeriveDataTypeable","DeriveGeneric","FlexibleInstances","MultiParamTypeClasses"]
    )
  , OptionsPragma src (Just GHC) " -fno-warn-unused-imports "
  ]
  where thPragma | templateHaskell = ["TemplateHaskell"]
                 | otherwise       = []

--------------------------------------------
-- OneofDescriptorProto module creation
--------------------------------------------
oneofModule :: Result -> OneofInfo -> Module
oneofModule result oi
  = Module src (ModuleName (fqMod protoName)) (modulePragmas $ oneofMakeLenses oi) Nothing
         Nothing imports (oneofDecls oi)
  where protoName = oneofName oi
        typs = mapMaybe typeName . F.toList . fmap snd . oneofFields $ oi
        imports = (standardImports False False (oneofMakeLenses oi))
                  ++ (mergeImports (mapMaybe (importPN result (ModuleName (fqMod protoName)) Normal) typs))


oneofDecls :: OneofInfo -> [Decl]
oneofDecls oi = (oneofX oi : oneofFuncs oi) ++ lenses ++ instances
  where
    mkPrisms = Var (Qual (ModuleName "Control.Lens.TH") (Ident "makePrisms"))
    lenses | oneofMakeLenses oi = [SpliceDecl src (mkFun $$ TypQuote (unqualName (oneofName oi))) |
                                   mkFun <- [mkLenses, mkPrisms]]
           | otherwise = []
    instances = [ instanceDefaultOneof oi
                , instanceMergeableOneof oi
                ]

oneofX :: OneofInfo -> Decl
oneofX oi = DataDecl src DataType [] (baseIdent (oneofName oi)) []
              (map oneofValueX (F.toList (oneofFields oi) ))
              derives
  where oneofValueX (pname,fi) = QualConDecl src [] [] con
          where con = RecDecl (baseIdent pname) [fieldX]
                fieldX = ([baseIdent' . fieldName $ fi], TyParen (TyCon typed ))
                typed = case useType (getFieldType (typeCode fi)) of
                          Just s -> private s
                          Nothing -> case typeName fi of
                                       Just s -> qualName s
                                       Nothing -> imp $ "No Name for Field!\n" ++ show fi

oneofFuncs :: OneofInfo -> [Decl]
oneofFuncs oi = map mkfuns (F.toList (oneofFields oi))
  where mkfuns f = defun (fst (oneofGet f)) [patvar "x"] $
                     Case (lvar "x")
                       [ Alt src (snd (oneofPat f))
                         (UnGuardedRhs (preludecon "Just" $$ snd (oneofRec f))) noWhere
                       , Alt src PWildCard
                         (UnGuardedRhs (preludecon "Nothing")) noWhere
                       ]



{- oneof field does not have to have a default value, but for convenience
   (to make all messages an instance of Default and Mergeable), we make
   the first case as default like enum. -}

instanceDefaultOneof :: OneofInfo -> Decl
instanceDefaultOneof oi
    =  InstDecl src Nothing [] [] (private "Default") [TyCon (unqualName (oneofName oi))]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: Exp
        firstValue = case Seq.viewl (oneofFields oi) of
                       EmptyL -> imp ("instanceDefaultOneof: empty in " ++ show oi)
                       (n,_) :< _ -> case (baseIdent n) of
                                       Ident str -> App (lcon str) (pvar "defaultValue")
                                       Symbol _ -> imp ("instanceDefaultOneof: " ++ show n)

instanceMergeableOneof :: OneofInfo -> Decl
instanceMergeableOneof oi
  = InstDecl src Nothing [] [] (private "Mergeable") [TyCon (unqualName (oneofName oi))] []


--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------
enumModule :: EnumInfo -> Module
enumModule ei
    = let protoName = enumName ei
      in Module src (ModuleName (fqMod protoName)) (modulePragmas False) Nothing
           (Just [EThingAll (unqualName protoName)])
           (standardImports True False False) (enumDecls ei)

enumDecls :: EnumInfo -> [Decl]
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
                           , instanceTextTypeEnum
                           ]

enumX :: EnumInfo -> Decl
enumX ei = DataDecl src DataType [] (baseIdent (enumName ei)) [] (map enumValueX (enumValues ei)) derivesEnum
--  where enumValueX (_,name) = ConDecl src (HsIdent name) []
  where enumValueX (_,name) = QualConDecl src [] [] (ConDecl (Ident name) [])

instanceTextTypeEnum :: EnumInfo -> Decl
instanceTextTypeEnum ei
  = InstDecl src Nothing [] [] (private "TextType") [TyCon (unqualName (enumName ei))] [
        inst "tellT" [] (pvar "tellShow")
      , inst "getT" [] (pvar "getRead")
      ]

instanceMergeableEnum :: EnumInfo -> Decl
instanceMergeableEnum ei
  = InstDecl src Nothing [] [] (private "Mergeable") [TyCon (unqualName (enumName ei))] []

instanceBounded :: EnumInfo -> Decl
instanceBounded ei
    = InstDecl src Nothing [] [] (prelude "Bounded") [TyCon (unqualName (enumName ei))]
         [set "minBound" (head values),set "maxBound" (last values)] -- values cannot be null in a well formed enum
  where values = enumValues ei
        set f (_,n) = inst f [] (lcon n)

{- from google's descriptor.h, about line 346:

  // Get the field default value if cpp_type() == CPPTYPE_ENUM.  If no
  // explicit default was defined, the default is the first value defined
  // in the enum type (all enum types are required to have at least one value).
  // This never returns NULL.

-}
instanceDefaultEnum :: EnumInfo -> Decl
instanceDefaultEnum ei
    = InstDecl src Nothing [] [] (private "Default") [TyCon (unqualName (enumName ei))]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: Exp
        firstValue = case enumValues ei of
                       (:) (_,n) _ -> lcon n
                       [] -> error $ "Impossible? EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show ei

declToEnum :: EnumInfo -> [Decl]
declToEnum ei = [ TypeSig src [Ident "toMaybe'Enum"]
                    (TyFun (TyCon (prelude "Int"))
                           (typeApp "Maybe" (TyCon (unqualName (enumName ei)))))
                , FunBind (map toEnum'one values ++ [final]) ]
  where values = enumValues ei
        toEnum'one (v,n) = match "toMaybe'Enum" [litIntP (getEnumCode v)] (preludecon "Just" $$ lcon n)
        final = match "toMaybe'Enum" [PWildCard] (preludecon "Nothing")

instanceEnum :: EnumInfo -> Decl
instanceEnum ei
    = InstDecl src Nothing [] [] (prelude "Enum") [TyCon (unqualName (enumName ei))]
        (map (InsDecl . FunBind) [fromEnum',toEnum',succ',pred'])
  where values = enumValues ei
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = match "fromEnum" [PApp (local n) []] (litInt (getEnumCode v))
        toEnum' = [ match "toEnum" [] (compose mayErr (lvar "toMaybe'Enum")) ]
        mayErr = pvar "fromMaybe" $$ (Paren (preludevar "error" $$  (litStr $
                   "hprotoc generated code: toEnum failure for type "++ fqMod (enumName ei))))
        succ' = zipWith (equate "succ") values (tail values) ++
                [ match "succ" [PWildCard] (preludevar "error" $$  (litStr $
                   "hprotoc generated code: succ failure for type "++ fqMod (enumName ei))) ]
        pred' = zipWith (equate "pred") (tail values) values ++
                [ match "pred" [PWildCard] (preludevar "error" $$  (litStr $
                   "hprotoc generated code: pred failure for type "++ fqMod (enumName ei))) ]
        equate f (_,n1) (_,n2) = match f [PApp (local n1) []] (lcon n2)

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: EnumInfo -> Decl
instanceWireEnum ei
    = InstDecl src Nothing [] [] (private "Wire") [TyCon (unqualName (enumName ei))]
        [ withName "wireSize", withName "wirePut", withGet, withGetErr,withGetPacked,withGetPackedErr ]
  where withName foo = inst foo [patvar "ft'",patvar "enum"] rhs
          where rhs = pvar foo $$ lvar "ft'" $$
                        (Paren $ preludevar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [litIntP' 14] rhs
          where rhs = pvar "wireGetEnum" $$ lvar "toMaybe'Enum"
        withGetErr = inst "wireGet" [patvar "ft'"] rhs
          where rhs = pvar "wireGetErr" $$ lvar "ft'"
        withGetPacked = inst "wireGetPacked" [litIntP' 14] rhs
          where rhs = pvar "wireGetPackedEnum" $$ lvar "toMaybe'Enum"
        withGetPackedErr = inst "wireGetPacked" [patvar "ft'"] rhs
          where rhs = pvar "wireGetErr" $$ lvar "ft'"

instanceGPB :: ProtoName -> Decl
instanceGPB protoName
    = InstDecl src Nothing [] [] (private "GPB") [TyCon (unqualName protoName)] []

instanceReflectEnum :: EnumInfo -> Decl
instanceReflectEnum ei
    = InstDecl src Nothing [] [] (private "ReflectEnum") [TyCon (unqualName (enumName ei))]
        [ inst "reflectEnum" [] ascList
        , inst "reflectEnumInfo" [ PWildCard ] ei' ]
  where (ProtoName xxx a b c) = enumName ei
        xxx'Exp = Paren $ pvar "pack" $$ litStr (LC.unpack (utf8 (fiName xxx)))
        values = enumValues ei
        ascList,ei',protoNameExp :: Exp
        ascList = List (map one values)
          where one (v,ns) = Tuple Boxed [litInt (getEnumCode v),litStr ns,lcon ns]
        ei' = foldl' App (pcon "EnumInfo") [protoNameExp
                                             ,List $ map litStr (enumFilePath ei)
                                             ,List (map two values)]
          where two (v,ns) = Tuple Boxed [litInt (getEnumCode v),litStr ns]
        protoNameExp = Paren $ foldl' App (pvar "makePNF")
                                        [ xxx'Exp, mList a, mList b, litStr (mName c) ]
          where mList = List . map (litStr . mName)

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

--------------------------------------------
-- FileDescriptorProto module creation
--------------------------------------------

protoModule :: Result -> ProtoInfo -> ByteString -> Module
protoModule result pri fdpBS
  = let protoName = protoMod pri
        (extendees,myKeys) = unzip $ F.toList (extensionKeys pri)
        m = ModuleName (fqMod protoName)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
        exportNames = map (EVar . UnQual . Ident) ["protoInfo","fileDescriptorProto"]
#else
        exportKeys = map (EVar NoNamespace . unqualFName . fieldName) myKeys
        exportNames = map (EVar NoNamespace . UnQual . Ident) ["protoInfo","fileDescriptorProto"]
#endif
        imports = (protoImports ++) . mergeImports $
                    mapMaybe (importPN result m Normal) $
                      extendees ++ mapMaybe typeName myKeys
    in Module src m (modulePragmas False) Nothing (Just (exportKeys++exportNames)) imports
         (keysXTypeVal protoName (extensionKeys pri) ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
 where protoImports = standardImports False (not . Seq.null . extensionKeys $ pri) False ++
         [ ImportDecl src (ModuleName "Text.DescriptorProtos.FileDescriptorProto") False False False Nothing Nothing
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
                        (Just (False,[IAbs NoNamespace (Ident "FileDescriptorProto")]))
#else
                        (Just (False,[IAbs (Ident "FileDescriptorProto")]))
#endif
         , ImportDecl src (ModuleName "Text.ProtocolBuffers.Reflections") False False False Nothing Nothing
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
                        (Just (False,[IAbs NoNamespace (Ident "ProtoInfo")]))
#else
                        (Just (False,[IAbs (Ident "ProtoInfo")]))
#endif
         , ImportDecl src (ModuleName "Text.ProtocolBuffers.WireMessage") True False False Nothing (Just (ModuleName "P'"))
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
                        (Just (False,[IVar (Ident "wireGet,getFromBS")]))
#else
                        (Just (False,[IVar NoNamespace (Ident "wireGet,getFromBS")]))
#endif
         ]

embed'ProtoInfo :: ProtoInfo -> [Decl]
embed'ProtoInfo pri = [ myType, myValue ]
  where myType = TypeSig src [ Ident "protoInfo" ] (TyCon (local "ProtoInfo"))
        myValue = PatBind src (PApp (local "protoInfo") []) (UnGuardedRhs $
                    preludevar "read" $$ litStr (show pri)) noWhere

embed'fdpBS :: ByteString -> [Decl]
embed'fdpBS bs = [ myType, myValue ]
  where myType = TypeSig src [ Ident "fileDescriptorProto" ] (TyCon (local "FileDescriptorProto"))
        myValue = PatBind src (PApp (local "fileDescriptorProto") []) (UnGuardedRhs $
                    pvar "getFromBS" $$
                      Paren (pvar "wireGet" $$ litInt' 11) $$
                      Paren (pvar "pack" $$ litStr (LC.unpack bs))) noWhere

--------------------------------------------
-- DescriptorProto module creation
--------------------------------------------
descriptorModules :: Result -> DescriptorInfo -> [(FilePath,Module)]
descriptorModules result di
 = let mainPath = joinPath (descFilePath di)
       bootPath = joinPath (descFilePath di) ++ "-boot"
       keyfilePath = take (length mainPath - 3) mainPath ++ "'Key.hs"
   in (mainPath,descriptorNormalModule result di) :
      case getKind result (pKey (descName di)) of
        TopProtoInfo -> imp $ "descriptorModules was given a TopProtoInfo kinded DescriptorInfo!"
        Simple -> []
        TypeBoot -> [(bootPath,descriptorBootModule di)]
        KeyTypeBoot -> [(bootPath,descriptorKeyBootModule result di)]
        SplitKeyTypeBoot -> [(bootPath,descriptorBootModule di)
                           ,(keyfilePath,descriptorKeyfileModule result di)]

-- This build a hs-boot that declares the type of the data type only
descriptorBootModule :: DescriptorInfo -> Module
descriptorBootModule di
  = let protoName = descName di
        un = unqualName protoName
        classes = [prelude "Show",prelude "Eq",prelude "Ord",prelude "Typeable",prelude "Data", prelude "Generic"
                  ,private "Mergeable",private "Default"
                  ,private "Wire",private "GPB",private "ReflectDescriptor"
                  , private "TextType", private "TextMsg"
                  ]
                  ++ if hasExt di then [private "ExtendMessage"] else []
                  ++ if storeUnknown di then [private "UnknownMessage"] else []
        instMesAPI = InstDecl src Nothing [] [] (private "MessageAPI")
                       [TyVar (Ident "msg'"), TyFun (TyVar (Ident "msg'")) (TyCon un),  (TyCon un)] []
        dataDecl = DataDecl src DataType [] (baseIdent protoName) [] [] []
        mkInst s = InstDecl src Nothing [] [] s [TyCon un] []
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        eabs = EAbs NoNamespace un
#else
        eabs = EAbs un
#endif
    in Module src (ModuleName (fqMod protoName)) (modulePragmas $ makeLenses di) Nothing (Just [eabs]) minimalImports
         (dataDecl : instMesAPI : map mkInst classes)

-- This builds on the output of descriptorBootModule and declares a hs-boot that
-- declares the data type and the keys
descriptorKeyBootModule :: Result -> DescriptorInfo -> Module
descriptorKeyBootModule result di
  = let Module p1 m@(ModuleName _self) p3 p4 (Just exports) imports decls = descriptorBootModule di
        (extendees,myKeys) = unzip $ F.toList (keys di)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
#else
        exportKeys = map (EVar NoNamespace . unqualFName . fieldName) myKeys
#endif
        importTypes = mergeImports . mapMaybe (importPN result m Source) . nubSort $
                        extendees ++ mapMaybe typeName myKeys
        declKeys = keysXType (descName di) (keys di)
    in Module p1 m p3 p4 (Just (exports++exportKeys)) (imports++importTypes) (decls++declKeys)

-- This build the 'Key module that defines the keys only
descriptorKeyfileModule :: Result -> DescriptorInfo -> Module
descriptorKeyfileModule result di
  = let protoName'Key = (descName di) { baseName = MName . (++"'Key") . mName $ (baseName (descName di)) }
        (extendees,myKeys) = unzip $ F.toList (keys di)
        mBase = ModuleName (fqMod (descName di))
        m = ModuleName (fqMod protoName'Key)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
#else
        exportKeys = map (EVar NoNamespace . unqualFName . fieldName) myKeys
#endif
        importTypes = mergeImports . mapMaybe (importPN result mBase KeyFile) . nubSort $
                        extendees ++ mapMaybe typeName myKeys
        declKeys = keysXTypeVal protoName'Key (keys di)
    in Module src m (modulePragmas $ makeLenses di) Nothing (Just exportKeys) (minimalImports++importTypes) declKeys

-- This builds the normal module
descriptorNormalModule :: Result -> DescriptorInfo -> Module
descriptorNormalModule result di
  = let protoName = descName di
        un = unqualName protoName
        myKind = getKind result (pKey protoName)
        sepKey = myKind == SplitKeyTypeBoot
        (extendees,myKeys) = unzip $ F.toList (keys di)
        extendees' = if sepKey then [] else extendees
        myKeys' = if sepKey then [] else myKeys
        m = ModuleName (fqMod protoName)
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
#else
        exportKeys = map (EVar NoNamespace . unqualFName . fieldName) myKeys
#endif
        imports = (standardImports False (hasExt di) (makeLenses di) ++) . mergeImports . concat $
                    [ mapMaybe (importPN result m Normal) $
                        extendees' ++ mapMaybe typeName (myKeys' ++ (F.toList (fields di)))
                    , concat . mapMaybe (importO result m Normal) $ F.toList (descOneofs di)
                    , mapMaybe (importPFN result m) (map fieldName (myKeys ++ F.toList (knownKeys di))) ]
        lenses | makeLenses di = [SpliceDecl src (mkLenses $$ TypQuote (unqualName protoName))]
               | otherwise = []
        declKeys | sepKey = []
                 | otherwise = keysXTypeVal (descName di) (keys di)
    in Module src m (modulePragmas $ makeLenses di) Nothing (Just (EThingAll un : exportLenses di ++ exportKeys)) imports
         (descriptorX di : lenses ++ declKeys ++ instancesDescriptor di)

mkLenses :: Exp
mkLenses = Var (Qual (ModuleName "Control.Lens.TH") (Ident "makeLenses"))

exportLenses :: DescriptorInfo -> [ExportSpec]
exportLenses di =
  if makeLenses di
    then
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
      map (EVar . unqualFName . stripPrefix) (lensFieldNames di)
#else
      map (EVar NoNamespace . unqualFName . stripPrefix) (lensFieldNames di)
#endif
    else []
  where stripPrefix pfn = pfn { baseNamePrefix' = "" }
        lensFieldNames di = map fieldName (F.toList (fields di))
                            ++ map oneofFName (F.toList (descOneofs di))

minimalImports :: [ImportDecl]
minimalImports =
  [ ImportDecl src (ModuleName "Prelude") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Data.Typeable") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Data.Data") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "GHC.Generics") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Text.ProtocolBuffers.Header") True False False Nothing (Just (ModuleName "P'")) Nothing ]

standardImports :: Bool -> Bool -> Bool -> [ImportDecl]
standardImports isEnumMod ext lenses =
  [ ImportDecl src (ModuleName "Prelude") False False False Nothing Nothing (Just (False,ops))
  , ImportDecl src (ModuleName "Prelude") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Data.Typeable") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "GHC.Generics") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Data.Data") True False False Nothing (Just (ModuleName "Prelude'")) Nothing
  , ImportDecl src (ModuleName "Text.ProtocolBuffers.Header") True False False Nothing (Just (ModuleName "P'")) Nothing ] ++ lensTH
 where
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
       ops | ext = map (IVar . Symbol) $ base ++ ["==","<=","&&"]
           | otherwise = map (IVar . Symbol) base
#else
       ops | ext = map (IVar NoNamespace . Symbol) $ base ++ ["==","<=","&&"]
           | otherwise = map (IVar NoNamespace . Symbol) base
#endif
       base | isEnumMod = ["+","/","."]
            | otherwise = ["+","/"]
       lensTH | lenses = [ImportDecl src (ModuleName "Control.Lens.TH") True False False Nothing Nothing Nothing]
              | otherwise = []

keysXType :: ProtoName -> Seq KeyInfo -> [Decl]
keysXType self ks = map (makeKeyType self) . F.toList $ ks

keysXTypeVal :: ProtoName -> Seq KeyInfo -> [Decl]
keysXTypeVal self ks = concatMap (\ ki -> [makeKeyType self ki,makeKeyVal self ki]) . F.toList $ ks

makeKeyType :: ProtoName -> KeyInfo -> Decl
makeKeyType self (extendee,f) = keyType
  where keyType = TypeSig src [ baseIdent' . fieldName $ f ] (foldl1 TyApp . map TyCon $
                    [ private "Key", private labeled
                    , if extendee /= self then qualName extendee else unqualName extendee
                    , typeQName ])
        labeled | isPacked f = "PackedSeq"
                | canRepeat f = "Seq"
                | otherwise = "Maybe"
        typeNumber = getFieldType . typeCode $ f
        typeQName :: QName
        typeQName = case useType typeNumber of
                      Just s -> private s
                      Nothing -> case typeName f of
                                   Just s | self /= s -> qualName s
                                          | otherwise -> unqualName s
                                   Nothing -> error $  "No Name for Field!\n" ++ show f

makeKeyVal :: ProtoName -> KeyInfo -> Decl
makeKeyVal _self (_extendee,f) = keyVal
  where typeNumber = getFieldType . typeCode $ f
        keyVal = PatBind src (PApp (unqualFName . fieldName $ f) []) (UnGuardedRhs
                   (pcon "Key" $$ litInt (getFieldId (fieldNumber f))
                               $$ litInt typeNumber
                               $$ maybe (preludecon "Nothing")
                                        (Paren . (preludecon "Just" $$) . (defToSyntax (typeCode f)))
                                        (hsDefault f)
                   )) noWhere

defToSyntax :: FieldType -> HsDefault -> Exp
defToSyntax tc x =
  case x of
    HsDef'Bool b -> preludecon (show b)
    HsDef'ByteString bs -> (if tc == 9 then (\ xx -> Paren (pcon "Utf8" $$ xx)) else id) $
                           (Paren $ pvar "pack" $$ litStr (LC.unpack bs))
    HsDef'RealFloat (SRF'Rational r) | r < 0 -> Paren $ Lit (Frac r)
                                     | otherwise -> Lit (Frac r)
    HsDef'RealFloat SRF'nan  -> litInt'   0  /! litInt' 0
    HsDef'RealFloat SRF'ninf -> litInt'   1  /! litInt' 0
    HsDef'RealFloat SRF'inf  -> litInt' (-1) /! litInt' 0
    HsDef'Integer i -> litInt i
    HsDef'Enum s -> Paren $ preludevar "read" $$ litStr s
 where (/!) a b = Paren (mkOp "/" a b)

descriptorX :: DescriptorInfo -> Decl
descriptorX di = DataDecl src DataType [] name [] [QualConDecl src [] [] con] derives
  where self = descName di
        name = baseIdent self
        con = RecDecl name eFields
                where eFields = F.foldr ((:) . fieldX) end (fields di)
                      end = (if hasExt di then (extfield:) else id)
                            . (if storeUnknown di then (unknownField:) else id)
                            $ eOneof
                      eOneof = F.foldr ((:) . fieldOneofX) [] (descOneofs di)

        bangType = if lazyFields di then TyParen {- UnBangedTy -} else TyBang BangedTy . TyParen
        -- extfield :: ([Name],BangType)
        extfield = ([fieldIdent di "ext'field"], bangType (TyCon (private "ExtField")))
        -- unknownField :: ([Name],BangType)
        unknownField = ([fieldIdent di "unknown'field"], bangType (TyCon (private  "UnknownField")))
        -- fieldX :: FieldInfo -> ([Name],BangType)
        fieldX fi = ([baseIdent' . fieldName $ fi], bangType (labeled (TyCon typed)))
          where labeled | canRepeat fi = typeApp "Seq"
                        | isRequired fi = id
                        | otherwise = typeApp "Maybe"
                typed :: QName
                typed = case useType (getFieldType (typeCode fi)) of
                          Just s -> private s
                          Nothing -> case typeName fi of
                                       Just s | self /= s -> qualName s
                                              | otherwise -> unqualName s
                                       Nothing -> error $  "No Name for Field!\n" ++ show fi
        fieldOneofX :: OneofInfo -> ([Name],Type)
        fieldOneofX oi = ([baseIdent' . oneofFName $ oi], typeApp "Maybe" (TyParen (TyCon typed)))
          where typed = qualName (oneofName oi)

instancesDescriptor :: DescriptorInfo -> [Decl]
instancesDescriptor di = map ($ di) $
   (if hasExt di then (instanceExtendMessage:) else id) $
   (if storeUnknown di then (instanceUnknownMessage:) else id) $
   [ instanceMergeable
   , instanceDefault
   , instanceWireDescriptor
   , instanceMessageAPI . descName
   , instanceGPB . descName
   , instanceReflectDescriptor
   , instanceTextType
   , instanceTextMsg
   ]

instanceExtendMessage :: DescriptorInfo -> Decl
instanceExtendMessage di
    = InstDecl src Nothing [] [] (private "ExtendMessage") [TyCon (unqualName (descName di))]
        [ inst "getExtField" [] (Var (localField di "ext'field"))
        , inst "putExtField" [patvar "e'f", patvar "msg"] putextfield
        , inst "validExtRanges" [patvar "msg"] (pvar "extRanges" $$ (Paren $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = RecUpdate (lvar "msg") [ FieldUpdate (localField di "ext'field") (lvar "e'f") ]

instanceUnknownMessage :: DescriptorInfo -> Decl
instanceUnknownMessage di
    = InstDecl src Nothing [] [] (private "UnknownMessage") [TyCon (unqualName (descName di))]
        [ inst "getUnknownField" [] (Var (localField di "unknown'field"))
        , inst "putUnknownField" [patvar "u'f",patvar "msg"] putunknownfield
        ]
  where putunknownfield = RecUpdate (lvar "msg") [ FieldUpdate (localField di "unknown'field") (lvar "u'f") ]

instanceTextType :: DescriptorInfo -> Decl
instanceTextType di
  = InstDecl src Nothing [] [] (private "TextType") [TyCon (unqualName (descName di))] [
        inst "tellT" [] (pvar "tellSubMessage")
      , inst "getT" [] (pvar "getSubMessage")
      ]


instanceTextMsg :: DescriptorInfo -> Decl
instanceTextMsg di
  = InstDecl src Nothing [] [] (private "TextMsg") [TyCon (unqualName (descName di))] [
        inst "textPut" [patvar msgVar] genPrint
      , InsDecl $ FunBind [Match src (Ident "textGet") [] Nothing (UnGuardedRhs parser) bdecls]
      ]
  where
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        bdecls = Just (BDecls (subparsers ++ subparsersO))
#else
        bdecls = BDecls (subparsers ++ subparsersO)
#endif
        flds = F.toList (fields di)
        os = F.toList (descOneofs di)
        msgVar = distinctVar "msg"
        distinctVar var = if var `elem` reservedVars then distinctVar (var ++ "'") else var
        reservedVars = map toPrintName flds
        genPrintFields = map (Qualifier . printField msgVar) flds
        genPrintOneofs = map (Qualifier . printOneof msgVar) os
        genPrint = if null flds && null os
                   then preludevar "return" $$ Hse.Tuple Boxed []
                   else Do $ genPrintFields ++ genPrintOneofs

        parser
            | null flds && null os = preludevar "return" $$ pvar "defaultValue"
            | otherwise = Do [
                Generator src (patvar "mods")
                    $ pvar "sepEndBy"
                        $$ Paren (pvar "choice" $$ List (map (lvar . parserName) flds ++ map (lvar . parserNameO) os))
                        $$ pvar "spaces",
                Qualifier $ (preludevar "return")
                    $$ Paren (preludevar "foldl"
                        $$ Lambda src [patvar "v", patvar "f"] (lvar "f" $$ lvar "v")
                        $$ pvar "defaultValue"
                        $$ lvar "mods")
             ]
        parserName f = let Ident fname = baseIdent' (fieldName f) in "parse'" ++ fname
        parserNameO o = let Ident oname = baseIdent' (oneofFName o) in "parse'" ++ oname
        subparsers = map (\f -> defun (parserName f) [] (getField f)) flds
        getField fi = let printname = toPrintName fi
                          Ident funcname = baseIdent' (fieldName fi)
                          update = if canRepeat fi then pvar "append" $$ Paren (lvar funcname $$ lvar "o") $$ lvar "v" else lvar "v"
            in pvar "try" $$ Do [
                Generator src (patvar "v") $ pvar "getT" $$ litStr printname,
                Qualifier $ (preludevar "return")
                    $$ Paren (Lambda src [patvar "o"]
                        (RecUpdate (lvar "o") [ FieldUpdate (local funcname) update]))
            ]

        subparsersO = map funbind os
        funbind o = FunBind [Match src (Ident (parserNameO o)) [] Nothing (UnGuardedRhs (getOneof)) whereParse]
          where getOneof = pvar "try" $$
                             (pvar "choice" $$ List (map (Var . UnQual . Ident) parsefs))
                oflds = F.toList (oneofFields o)
                flds = map snd oflds
                parsefs = map parserName flds
                whereParse = whereBinds $ BDecls (map decl oflds)
                  where decl (n,f) = defun (parserName f) [] (getOneofField (n,f))
                        getOneofField p@(n,f) =
                          let Ident oname = baseIdent' (oneofFName o)
                              printname = toPrintName f
                              update = preludecon "Just" $$ Paren (oneofCon p $$ lvar "v")
                          in pvar "try" $$ Do [
                               Generator src (patvar "v") $ pvar "getT" $$ litStr printname,
                               Qualifier $ (preludevar "return")
                               $$ Paren (Lambda src [patvar "s"]
                                (RecUpdate (lvar "s") [ FieldUpdate (local oname) update]))
                               ]


printField :: String -> FieldInfo -> Exp
printField msgVar fi
  = let Ident funcname = baseIdent' (fieldName fi)
        printname = toPrintName fi
    in pvar "tellT" $$ litStr printname $$ Paren (lvar funcname $$ lvar msgVar)

toPrintName :: FieldInfo -> String
toPrintName fi = let IName uname = last $ splitFI $ protobufName' (fieldName fi) in uToString uname

printOneof :: String -> OneofInfo -> Exp
printOneof msgVar oi
    = Case (Paren (lvar funcname $$ lvar msgVar)) (map caseAlt flds ++ [caseAltNothing])
  where Ident funcname = baseIdent' (oneofFName oi)
        IName uname = last $ splitFI $ protobufName' (oneofFName oi)
        printname = uToString uname
        flds = F.toList (oneofFields oi)
        caseAlt :: (ProtoName,FieldInfo) -> Alt
        caseAlt f = Alt src patt  (UnGuardedRhs rhs) noWhere
          where patt = PApp (prelude "Just") [fst (oneofPat f)]
                (rstr,rvar) = oneofRec f
                rhs = pvar "tellT" $$ rstr $$ rvar -- litStr fname $$ (lvar fname)
        caseAltNothing :: Alt
        caseAltNothing = Alt src (PApp (prelude "Nothing") []) (UnGuardedRhs rhs) noWhere
          where rhs = preludevar "return" $$ unit_con

instanceMergeable :: DescriptorInfo -> Decl
instanceMergeable di
    = InstDecl src Nothing [] [] (private "Mergeable") [TyCon un]
        [ -- inst "mergeEmpty" [] (foldl' App (Con un) (replicate len (pvar "mergeEmpty"))),
          inst "mergeAppend" [PApp un patternVars1, PApp un patternVars2]
                             (foldl' App (Con un) (zipWith append vars1 vars2))
        ]
  where un = unqualName (descName di)
        len = (if hasExt di then succ else id)
            $ (if storeUnknown di then succ else id)
            $ Seq.length (fields di) + Seq.length (descOneofs di)
        patternVars1,patternVars2 :: [Pat]
        patternVars1 = take len inf
            where inf = map (\ n -> patvar ("x'" ++ show n)) [(1::Int)..]
        patternVars2 = take len inf
            where inf = map (\ n -> patvar ("y'" ++ show n)) [(1::Int)..]
        vars1,vars2 :: [Exp]
        vars1 = take len inf
            where inf = map (\ n -> lvar ("x'" ++ show n)) [(1::Int)..]
        vars2 = take len inf
            where inf = map (\ n -> lvar ("y'" ++ show n)) [(1::Int)..]
        append x y = Paren $ pvar "mergeAppend" $$ x $$ y

instanceDefault :: DescriptorInfo -> Decl
instanceDefault di
    = InstDecl src Nothing [] [] (private "Default") [TyCon un]
        [ inst "defaultValue" [] (foldl' App (Con un) deflistExt) ]
  where un = unqualName (descName di)
        deflistExt = F.foldr ((:) . defX) end (fields di)
        end = (if hasExt di then (pvar "defaultValue":) else id)
            . (if storeUnknown di then (pvar "defaultValue":) else id)
            $ F.foldr ((:) . defOneof) [] (descOneofs di)

        defX :: FieldInfo -> Exp
        defX fi | isRequired fi = dv1
                | otherwise = dv2
          where dv1 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> defToSyntax (typeCode fi) hsdef
                dv2 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> Paren $ preludecon "Just" $$ defToSyntax (typeCode fi) hsdef
        defOneof :: OneofInfo -> Exp
        defOneof oi= pvar "defaultValue"


instanceMessageAPI :: ProtoName -> Decl
instanceMessageAPI protoName
    = InstDecl src Nothing [] [] (private "MessageAPI")
        [TyVar (Ident "msg'"), TyFun (TyVar (Ident "msg'")) (TyCon un),  (TyCon un)]
        [ inst "getVal" [patvar "m'",patvar "f'"] (App (lvar "f'" ) (lvar "m'")) ]
  where un = unqualName protoName

instanceWireDescriptor :: DescriptorInfo -> Decl
instanceWireDescriptor di@(DescriptorInfo { descName = protoName
                                          , fields = fieldInfos
                                          , descOneofs = oneofInfos
                                          , extRanges = allowedExts
                                          , knownKeys = fieldExts })
  = let me = unqualName protoName
        extensible = not (null allowedExts)
        len = (if extensible then succ else id)
            $ (if storeUnknown di then succ else id)
            $ Seq.length fieldInfos + Seq.length oneofInfos
        mine = PApp me . take len . map (\ n -> patvar ("x'" ++ show n)) $ [(1::Int)..]
        vars = take len . map (\ n -> lvar ("x'" ++ show n)) $ [(1::Int)..]
        mExt | extensible = Just (vars !! Seq.length fieldInfos)
             | otherwise = Nothing
        mUnknown | storeUnknown di = Just (last vars)
                 | otherwise = Nothing

-- reusable 'cases' generator
        -- first case is for Group behavior, second case is for Message behavior, last is error handler
        cases g m e = Case (lvar "ft'") [ Alt src (litIntP' 10) (UnGuardedRhs g) noWhere
                                        , Alt src (litIntP' 11) (UnGuardedRhs m) noWhere
                                        , Alt src PWildCard     (UnGuardedRhs e) noWhere
                                        ]

-- wireSize generation
        sizeCases = UnGuardedRhs $ cases (lvar "calc'Size")
                                         (pvar "prependMessageSize" $$ lvar "calc'Size")
                                         (pvar "wireSizeErr" $$ lvar "ft'" $$ lvar "self'")
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        whereCalcSize = Just (BDecls [defun "calc'Size" [] sizes])
#else
        whereCalcSize = BDecls [defun "calc'Size" [] sizes]
#endif
        sizes | null sizesList = Lit (Hse.Int 0)
              | otherwise = Paren (foldl1' (+!) sizesList)
          where (+!) = mkOp "+"
                sizesList | Just v <- mUnknown = sizesListExt ++ [ pvar "wireSizeUnknownField" $$ v ]
                          | otherwise = sizesListExt
                sizesListExt | Just v <- mExt = sizesListFields ++ [ pvar "wireSizeExtField" $$ v ]
                             | otherwise = sizesListFields
                sizesListFields =  concat . zipWith toSize vars . F.toList $
                                     fmap Left fieldInfos >< fmap Right oneofInfos
        toSize var (Left fi)
          = let f = if isPacked fi then "wireSizePacked"
                    else if isRequired fi then "wireSizeReq"
                         else if canRepeat fi then "wireSizeRep"
                              else "wireSizeOpt"
            in [foldl' App (pvar f) [ litInt (wireTagLength fi)
                                    , litInt (getFieldType (typeCode fi))
                                    , var]]
        toSize var (Right oi) = map (toSize' var) . F.toList . oneofFields $ oi
          where toSize' var r@(n,fi)
                  = let f = "wireSizeOpt"
                        var' = mkOp "Prelude'.=<<" (Var (qualName (snd (oneofGet r)))) var
                    in foldl' App (pvar f) [ litInt (wireTagLength fi)
                                           , litInt (getFieldType (typeCode fi))
                                           , var']


-- wirePut generation
        putCases = UnGuardedRhs $ cases
          (lvar "put'Fields")
          (Do [ Qualifier $ pvar "putSize" $$
                    (Paren $ foldl' App (pvar "wireSize") [ litInt' 10 , lvar "self'" ])
                , Qualifier $ lvar "put'Fields" ])
          (pvar "wirePutErr" $$ lvar "ft'" $$ lvar "self'")
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        wherePutFields = Just (BDecls [defun "put'Fields" [] (Do putStmts)])
#else
        wherePutFields = BDecls [defun "put'Fields" [] (Do putStmts)]
#endif
        putStmts = putStmtsContent
          where putStmtsContent | null putStmtsAll = [Qualifier $ preludevar "return" $$ Con (Special UnitCon)]
                                | otherwise = putStmtsAll
                putStmtsAll | Just v <- mUnknown = putStmtsListExt ++ [ Qualifier $ pvar "wirePutUnknownField" $$ v ]
                             | otherwise = putStmtsListExt
                putStmtsListExt | Just v <- mExt = sortedPutStmtsList ++ [ Qualifier $ pvar "wirePutExtField" $$ v ]
                                | otherwise = sortedPutStmtsList
                sortedPutStmtsList = map snd                                          -- remove number
                                     . sortBy (compare `on` fst)                      -- sort by number
                                     $ putStmtsList
                putStmtsList = concat . zipWith toPut vars . F.toList $
                                 fmap Left fieldInfos >< fmap Right oneofInfos
        toPut var (Left fi)
          = let f = if isPacked fi then "wirePutPacked"
                    else if isRequired fi then "wirePutReq"
                         else if canRepeat fi then "wirePutRep"
                              else "wirePutOpt"
            in [(fieldNumber fi,
                 Qualifier $
                   foldl' App (pvar f) [ litInt (getWireTag (wireTag fi))
                                       , litInt (getFieldType (typeCode fi))
                                       , var]
                 )]
        toPut var (Right oi) = map (toPut' var) . F.toList . oneofFields $ oi
          where toPut' var r@(n,fi)
                  = let f = "wirePutOpt"
                        var' = mkOp "Prelude'.=<<" (Var (qualName (snd (oneofGet r)))) var
                    in (fieldNumber fi
                       ,Qualifier $
                          foldl' App (pvar f) [ litInt (getWireTag (wireTag fi))
                                              , litInt (getFieldType (typeCode fi))
                                              , var']
                       )

-- wireGet generation
-- new for 1.5.7, rewriting this a great deal!
        getCases = let param = if storeUnknown di
                                 then Paren (pvar "catch'Unknown" $$ lvar "update'Self")
                                 else lvar "update'Self"
                   in UnGuardedRhs $ cases (pvar "getBareMessageWith" $$ param)
                                           (pvar "getMessageWith" $$ param)
                                           (pvar "wireGetErr" $$ lvar "ft'")
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        whereDecls = Just (BDecls [whereUpdateSelf])
#else
        whereDecls = BDecls [whereUpdateSelf]
#endif
        whereUpdateSelf = defun "update'Self" [patvar "wire'Tag", patvar "old'Self"]
                                (Case (lvar "wire'Tag") updateAlts)
        -- update cases are all normal fields then all known extensions then wildcard
        updateAlts = concatMap toUpdate (F.toList fieldInfos)
                     ++ (do -- in list monad
                          o <- F.toList oneofInfos
                          f <- F.toList (oneofFields o)
                          toUpdateO o f)
                     ++ (if extensible then concatMap toUpdateExt (F.toList fieldExts) else [])
                     ++ [Alt src PWildCard (UnGuardedRhs wildcardAlt) noWhere]
        -- the wildcard alternative handles new extensions and
        wildcardAlt = letPair extBranch
          where letPair = Let (BDecls [PatBind src (PTuple Boxed [patvar "field'Number",patvar "wire'Type"])
                                         (UnGuardedRhs (pvar "splitWireTag" $$ lvar "wire'Tag")) bdecls])
                extBranch | extensible = If (isAllowedExt (lvar "field'Number"))
                                            (argPair (pvar "loadExtension"))
                                            unknownBranch
                          | otherwise = unknownBranch
                unknownBranch = argPair (pvar "unknown")
                argPair x = x $$ lvar "field'Number" $$ lvar "wire'Type" $$ lvar "old'Self"
#if MIN_VERSION_haskell_src_exts(1, 17, 0)
        bdecls = Nothing
#else
        bdecls = BDecls []
#endif
        isAllowedExt x = preludevar "or" $$ List ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&"; (==!) = mkOp "=="; (FieldId maxHi) = maxBound
          ranges = map (\ (FieldId lo,FieldId hi) ->
                            if hi < maxHi
                              then if lo == hi
                                     then (x ==! litInt lo)
                                     else (litInt lo <=! x) &&! (x <=! litInt hi)
                              else litInt lo <=! x )
                       allowedExts

-- wireGetErr for known extensions
-- need to check isPacked and call appropriate wireGetKey[Un]Packed substitute function
        toUpdateExt fi | Just (wt1,wt2) <- packedTag fi = [toUpdateExtUnpacked wt1, toUpdateExtPacked wt2]
                       | otherwise = [toUpdateExtUnpacked (wireTag fi)]
          where (getUnP,getP) | isPacked fi = (pvar "wireGetKeyToPacked",pvar "wireGetKey")
                              | otherwise = (pvar "wireGetKey",pvar "wireGetKeyToUnPacked")
                toUpdateExtUnpacked wt1 =
                  Alt src (litIntP . getWireTag $ wt1)
                      (UnGuardedRhs $ getUnP $$ Var (mayQualName protoName (fieldName fi)) $$ lvar "old'Self")
                      noWhere
                toUpdateExtPacked wt2 =
                  Alt src (litIntP . getWireTag $ wt2)
                      (UnGuardedRhs $ getP $$ Var (mayQualName protoName (fieldName fi)) $$ lvar "old'Self")
                      noWhere

-- wireGet without extensions
        toUpdate fi | Just (wt1,wt2) <- packedTag fi = [toUpdateUnpacked wt1 fi, toUpdatePacked wt2 fi]
                    | otherwise                      = [toUpdateUnpacked (wireTag fi) fi]



        toUpdateUnpacked wt1 fi =
          Alt src (litIntP . getWireTag $ wt1) (UnGuardedRhs $
            preludevar "fmap" $$ (Paren $ Lambda src [PBangPat (patvar "new'Field")] $
                              RecUpdate (lvar "old'Self")
                                        [FieldUpdate (unqualFName . fieldName $ fi)
                                                     (labelUpdateUnpacked fi)])
                        $$ (Paren (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdateUnpacked fi | canRepeat fi = pvar "append" $$ Paren ((Var . unqualFName . fieldName $ fi)
                                                                             $$ lvar "old'Self")
                                                              $$ lvar "new'Field"
                               | isRequired fi = qMerge (lvar "new'Field")
                               | otherwise = qMerge (preludecon "Just" $$ lvar "new'Field")
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,(11::Int)] =
                               pvar "mergeAppend" $$ Paren ( (Var . unqualFName . fieldName $ fi)
                                                               $$ lvar "old'Self" )
                                                  $$ Paren x
                           | otherwise = x
        toUpdatePacked wt2 fi =
          Alt src (litIntP . getWireTag $ wt2) (UnGuardedRhs $
            preludevar "fmap" $$ (Paren $ Lambda src [PBangPat (patvar "new'Field")] $
                              RecUpdate (lvar "old'Self")
                                        [FieldUpdate (unqualFName . fieldName $ fi)
                                                     (labelUpdatePacked fi)])
                        $$ (Paren (pvar "wireGetPacked" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdatePacked fi = pvar "mergeAppend" $$ Paren ((Var . unqualFName . fieldName $ fi)
                                                                 $$ lvar "old'Self")
                                                  $$ lvar "new'Field"

        -- in the above, the [10,11] check optimizes using the
        -- knowledge that only TYPE_MESSAGE and TYPE_GROUP have merges
        -- that are not right-biased replacements.  The "mergeAppend" uses
        -- knowledge of how all repeated fields get merged.


        -- for fields in OneofInfo
        toUpdateO oi f@(_n,fi)
          | Just (wt1,wt2) <- packedTag fi = [toUpdateUnpackedO oi wt1 f, toUpdatePackedO oi wt2 f]
          | otherwise                      = [toUpdateUnpackedO oi (wireTag fi) f]

        toUpdateUnpackedO oi wt1 f@(_,fi) =
          Alt src (litIntP . getWireTag $ wt1) (UnGuardedRhs $
            preludevar "fmap" $$ (Paren $ Lambda src [PBangPat (patvar "new'Field")] $
                              RecUpdate (lvar "old'Self")
                                        [FieldUpdate (unqualFName . oneofFName $ oi)
                                                     (labelUpdateUnpackedO oi f)])
                        $$ (Paren (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdateUnpackedO oi f@(_,fi) = qMerge (preludecon "Just" $$
                                               (oneofCon f $$ lvar "new'Field")
                                            )
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,(11::Int)] =
                               pvar "mergeAppend" $$ Paren ( (Var . unqualFName . oneofFName $ oi)
                                                               $$ lvar "old'Self" )
                                                  $$ Paren x
                           | otherwise = x
        toUpdatePackedO oi wt2 f@(_,fi) =
          Alt src (litIntP . getWireTag $ wt2) (UnGuardedRhs $
            preludevar "fmap" $$ (Paren $ Lambda src [PBangPat (patvar "new'Field")] $
                              RecUpdate (lvar "old'Self")
                                        [FieldUpdate (unqualFName . oneofFName $ oi)
                                                     (labelUpdatePackedO oi f)])
                        $$ (Paren (pvar "wireGetPacked" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdatePackedO oi f@(_,fi) = pvar "mergeAppend" $$ Paren ((Var . unqualFName . oneofFName $ oi)
                                                                 $$ lvar "old'Self")
                                                  $$ Paren (preludecon "Just" $$
                                                              (oneofCon f $$ lvar "new'Field"))




    in InstDecl src Nothing [] [] (private "Wire") [TyCon me] . map InsDecl $
        [ FunBind [Match src (Ident "wireSize") [patvar "ft'",PAsPat (Ident "self'") (PParen mine)] Nothing sizeCases whereCalcSize]
        , FunBind [Match src (Ident "wirePut")  [patvar "ft'",PAsPat (Ident "self'") (PParen mine)] Nothing putCases wherePutFields]
        , FunBind [Match src (Ident "wireGet") [patvar "ft'"] Nothing getCases whereDecls]
        ]

instanceReflectDescriptor :: DescriptorInfo -> Decl
instanceReflectDescriptor di
    = InstDecl src Nothing [] [] (private "ReflectDescriptor") [TyCon (unqualName (descName di))]
        [ inst "getMessageInfo" [PWildCard] gmi
        , inst "reflectDescriptorInfo" [ PWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: Exp
        rdi = preludevar "read" $$ litStr (show di)
        gmi,reqId,allId :: Exp
        gmi = pcon "GetMessageInfo" $$ Paren reqId $$ Paren allId
        reqId = pvar "fromDistinctAscList" $$
                List (map litInt . sort . concat $ [ allowedList fi | fi <- F.toList (fields di), isRequired fi])
        allId = pvar "fromDistinctAscList" $$
                List (map litInt . sort . concat $ [ allowedList fi | fi <- F.toList (fields di)] ++
                                                   [ allowedList fi | fi <- F.toList (knownKeys di)])
        allowedList fi | Just (wt1,wt2) <- packedTag fi = [getWireTag wt1,getWireTag wt2]
                       | otherwise = [getWireTag (wireTag fi)]

------------------------------------------------------------------

derives,derivesEnum :: [Deriving]
derives = map (\ x -> (prelude x,[])) ["Show","Eq","Ord","Typeable","Data","Generic"]
derivesEnum = map (\ x -> (prelude x,[])) ["Read","Show","Eq","Ord","Typeable","Data","Generic"]

-- All of these type names are also exported by Text.ProtocolBuffers.Header via Text.ProtocolBuffers.Basic
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
useType  x = imp $ "useType: Unknown type code (expected 1 to 18) of "++show x


