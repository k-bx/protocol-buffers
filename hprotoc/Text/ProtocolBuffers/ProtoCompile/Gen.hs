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
module Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModules,enumModule,prettyPrint) where

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Reflections(KeyInfo,HsDefault(..),DescriptorInfo(..),ProtoInfo(..),EnumInfo(..),ProtoName(..),ProtoFName(..),FieldInfo(..))

import Text.ProtocolBuffers.ProtoCompile.BreakRecursion(Result(..),VertexKind(..),pKey,pfKey,getKind,Part(..))

import qualified Data.ByteString.Lazy.Char8 as LC(unpack)
import qualified Data.Foldable as F(foldr,toList)
import Data.List(sortBy,foldl',foldl1',group,sort,union)
import Data.Function(on)
import Language.Haskell.Exts.Pretty(prettyPrint)
import Language.Haskell.Exts.Syntax hiding (Int,String)
import Language.Haskell.Exts.Syntax as Hse
import qualified Data.Map as M
import Data.Maybe(mapMaybe)
import qualified Data.Sequence as Seq(null,length)
import qualified Data.Set as S
import System.FilePath(joinPath)

import Debug.Trace(trace)


default (Int)

-- -- -- -- Helper functions

imp :: String -> a
imp s = error ("Impossible? Text.ProtocolBuffers.ProtoCompile.Gen."++s)

nubSort :: Ord a => [a] -> [a]
nubSort = map head . group . sort

noWhere :: Binds
noWhere = BDecls []

($$) :: Exp -> Exp -> Exp
($$) = App

infixl 1 $$

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

litStr :: String -> Exp
litStr = Lit . Hse.String

litIntP :: Integral x => x -> Pat
litIntP x | x<0 = PParen $ PLit (Hse.Int (toInteger x))
          | otherwise = PLit (Hse.Int (toInteger x))

litInt :: Integral x => x -> Exp
litInt x | x<0 = Paren $ Lit (Hse.Int (toInteger x))
         | otherwise = Lit (Hse.Int (toInteger x))

typeApp :: String -> Type -> Type
typeApp s =  TyApp (TyCon (private s))

private :: String -> QName
private t = Qual (ModuleName "P'") (Ident t)

local :: String -> QName
local t = UnQual (Ident t)

pvar :: String -> Exp
pvar t = Var (private t)

pcon :: String -> Exp
pcon t = Con (private t)

lvar :: String -> Exp
lvar t = Var (local t)

lcon :: String -> Exp
lcon t = Con (local t)

patvar :: String -> Pat
patvar t = PVar (Ident t)

match :: String -> [Pat] -> Exp -> Match
match s p r = Match src (Ident s) p (UnGuardedRhs r) noWhere

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

-- importPN takes the Result to look up the target info it takes the
-- current MKey (pKey of protoName, no 'Key appended) and Part to
-- identify the module being created.  The ProtoName is the target
-- TYPE that is needed.
importPN :: Result -> ModuleName -> Part -> ProtoName -> Maybe ImportDecl
importPN r selfMod@(ModuleName self) part pn =
  let o = pKey pn
      m1 = ModuleName (joinMod (haskellPrefix pn ++ parentModule pn ++ [baseName pn]))
      m2 = ModuleName (joinMod (parentModule pn))
-- ZZZ      fromSource = canSource && S.member (FMName self,o) (rIBoot r)
      fromSource = S.member (FMName self,part,o) (rIBoot r) -- ZZZ
      ans = if m1 == selfMod then Nothing
              else Just $ ImportDecl src m1 True fromSource (Just m2)
                            (Just (False,[IAbs (Ident (mName (baseName pn)))]))
  in trace (unlines . map (\ (a,b) -> a ++ " = "++b) $
                 [("selfMod",show selfMod)
                 ,("part",show part)
                 ,("pn",show pn)
                 ,("o",show o)
                 ,("m1",show m1)
                 ,("m2",show m2)
                 ,("fromSource",show fromSource)
                 ,("ans",show ans)]) $
     ans

-- ZZZ below
importPFN :: Result -> ModuleName -> ProtoFName -> Maybe ImportDecl
importPFN r m@(ModuleName self) pfn =
  let o@(FMName other) = pfKey pfn
      m1@(ModuleName m1') = ModuleName (joinMod (haskellPrefix' pfn ++ parentModule' pfn))
      m2 = ModuleName (joinMod (parentModule' pfn))
      qualified = (m1 /= m2) && (m1 /= m)
      spec = Just (False,[IVar (Ident (fName (baseName' pfn)))])
      kind = getKind r o
      fromAlt = S.member (FMName self,FMName m1') (rIKey r)
      ans = if m1 == m && kind /= SplitKeyTypeBoot then Nothing else Just $
              case kind of
                KeyTypeBoot -> ImportDecl src m1 qualified fromAlt (if qualified then Just m2 else Nothing) spec
                SplitKeyTypeBoot | fromAlt -> ImportDecl src (keyFile m1) qualified False (if qualified then Just m2 else Nothing) spec
                TopProtoInfo -> imp $ "importPFN from the TopProtoInfo module"
                _ -> ImportDecl src m1 qualified False (if qualified then Just m2 else Nothing) spec
  in trace (unlines . map (\ (a,b) -> a ++ " = "++b) $
                [("m",show m)
                ,("pfn",show pfn)
                ,("o",show o)
                ,("m1",show m1)
                ,("m2",show m2)
                ,("kind",show kind)
                ,("ans",show ans)]) $
     ans


-- Several items might be taken from the same module, combine these statements
mergeImports :: [ImportDecl] -> [ImportDecl]
mergeImports importsIn =
  let idKey (ImportDecl _p1 p2 p3 p4 p5 (Just (p6,_xs))) = (p2,p3,p4,p5,Just p6)
      idKey (ImportDecl _p1 p2 p3 p4 p5 Nothing) = (p2,p3,p4,p5,Nothing)
      mergeImports (ImportDecl p1 p2 p3 p4 p5 (Just (p6,xs)))
                   (ImportDecl _ _ _ _ _ (Just (_,ys))) =
        ImportDecl p1 p2 p3 p4 p5 (Just (p6,xs `union` ys))
      mergeImports i _ = i -- identical, so drop one
      combined = M.fromListWith mergeImports . map (\ i -> (idKey i,i)) $ importsIn
  in M.elems combined

keyFile :: ModuleName -> ModuleName
keyFile (ModuleName s) = ModuleName (s++"'Key")

joinMod :: [MName String] -> String
joinMod [] = ""
joinMod ms = fmName $ foldr1 dotFM . map promoteFM $ ms

baseIdent :: ProtoName -> Name
baseIdent = Ident . mName . baseName
baseIdent' :: ProtoFName -> Name
baseIdent' = Ident . fName . baseName'

qualName :: ProtoName -> QName
qualName p@(ProtoName _ _prefix [] _base) = UnQual (baseIdent p)
qualName p@(ProtoName _ _prefix (parents) _base) = Qual (ModuleName (joinMod parents)) (baseIdent p)

qualFName :: ProtoFName -> QName
qualFName p@(ProtoFName _ _prefix [] _base) = UnQual (baseIdent' p)
qualFName p@(ProtoFName _ _prefix parents _base) = Qual (ModuleName (joinMod parents)) (baseIdent' p)

unqualName :: ProtoName -> QName
unqualName p = UnQual (baseIdent p)

unqualFName :: ProtoFName -> QName
unqualFName p = UnQual (baseIdent' p)

mayQualName :: ProtoName -> ProtoFName -> QName
mayQualName (ProtoName _ c'prefix c'parents c'base) name@(ProtoFName _ prefix parents _base) =
  if joinMod (c'prefix++c'parents++[c'base]) == joinMod (prefix++parents)
    then UnQual (baseIdent' name) -- name is local, make UnQual
    else qualFName name           -- name is imported, make Qual

--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------
enumModule :: EnumInfo -> Module
enumModule ei
    = let protoName = enumName ei
      in Module src (ModuleName (fqMod protoName)) [] Nothing
           (Just [EThingAll (unqualName protoName)])
           (standardImports True False) (enumDecls ei)

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
                           ]

enumX :: EnumInfo -> Decl
enumX ei = DataDecl src DataType [] (baseIdent (enumName ei)) [] (map enumValueX (enumValues ei)) derivesEnum
--  where enumValueX (_,name) = ConDecl src (HsIdent name) []
  where enumValueX (_,name) = QualConDecl src [] [] (ConDecl (Ident name) [])

instanceMergeableEnum :: EnumInfo -> Decl
instanceMergeableEnum ei 
  = InstDecl src [] (private "Mergeable") [TyCon (unqualName (enumName ei))] []

instanceBounded :: EnumInfo -> Decl
instanceBounded ei
    = InstDecl src [] (private "Bounded") [TyCon (unqualName (enumName ei))] 
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
    = InstDecl src [] (private "Default") [TyCon (unqualName (enumName ei))]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: Exp
        firstValue = case enumValues ei of
                       (:) (_,n) _ -> lcon n
                       [] -> error $ "Impossible? EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show ei

declToEnum :: EnumInfo -> [Decl]
declToEnum ei = [ TypeSig src [Ident "toMaybe'Enum"]
                    (TyFun (TyCon (private "Int"))
                           (typeApp "Maybe" (TyCon (unqualName (enumName ei)))))
                , FunBind (map toEnum'one values ++ [final]) ]
  where values = enumValues ei
        toEnum'one (v,n) = match "toMaybe'Enum" [litIntP (getEnumCode v)] (pcon "Just" $$ lcon n)
        final = match "toMaybe'Enum" [PWildCard] (pcon "Nothing")

instanceEnum :: EnumInfo -> Decl
instanceEnum ei
    = InstDecl src [] (private "Enum") [TyCon (unqualName (enumName ei))]
        (map (InsDecl . FunBind) [fromEnum',toEnum',succ',pred'])
  where values = enumValues ei
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = match "fromEnum" [PApp (local n) []] (litInt (getEnumCode v))
        toEnum' = [ match "toEnum" [] (compose mayErr (lvar "toMaybe'Enum")) ]
        mayErr = pvar "fromMaybe" $$ (Paren (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: toEnum failure for type "++ fqMod (enumName ei))))
        succ' = zipWith (equate "succ") values (tail values) ++
                [ match "succ" [PWildCard] (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: succ failure for type "++ fqMod (enumName ei))) ]
        pred' = zipWith (equate "pred") (tail values) values ++
                [ match "pred" [PWildCard] (pvar "error" $$  (litStr $ 
                   "hprotoc generated code: pred failure for type "++ fqMod (enumName ei))) ]
        equate f (_,n1) (_,n2) = match f [PApp (local n1) []] (lcon n2)

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: EnumInfo -> Decl
instanceWireEnum ei
    = InstDecl src [] (private "Wire") [TyCon (unqualName (enumName ei))]
        [ withName "wireSize", withName "wirePut", withGet, withGetErr ]
  where withName foo = inst foo [patvar "ft'",patvar "enum"] rhs
          where rhs = pvar foo $$ lvar "ft'" $$
                        (Paren $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [litIntP 14] rhs
          where rhs = pvar "wireGetEnum" $$ lvar "toMaybe'Enum"
        withGetErr = inst "wireGet" [patvar "ft'"] rhs
          where rhs = pvar "wireGetErr" $$ lvar "ft'"

instanceGPB :: ProtoName -> Decl
instanceGPB protoName
    = InstDecl src [] (private "GPB") [TyCon (unqualName protoName)] []

instanceReflectEnum :: EnumInfo -> Decl
instanceReflectEnum ei
    = InstDecl src [] (private "ReflectEnum") [TyCon (unqualName (enumName ei))]
        [ inst "reflectEnum" [] ascList
        , inst "reflectEnumInfo" [ PWildCard ] ei' ]
  where (ProtoName xxx a b c) = enumName ei
        xxx'Exp = Paren $ pvar "pack" $$ litStr (LC.unpack (utf8 (fiName xxx)))
        values = enumValues ei
        ascList,ei',protoNameExp :: Exp
        ascList = List (map one values)
          where one (v,ns) = Tuple [litInt (getEnumCode v),litStr ns,lcon ns]
        ei' = foldl' App (pcon "EnumInfo") [protoNameExp
                                             ,List $ map litStr (enumFilePath ei)
                                             ,List (map two values)]
          where two (v,ns) = Tuple [litInt (getEnumCode v),litStr ns]
        protoNameExp = Paren $ foldl' App (pvar "makePNF")
                                        [ xxx'Exp, mList a, mList b, litStr (mName c) ]
          where mList = List . map (litStr . mName)

hasExt :: DescriptorInfo -> Bool
hasExt di = not (null (extRanges di))

--------------------------------------------
-- FileDescriptorProto module creation
--------------------------------------------

protoModule :: Result -> ProtoInfo -> ByteString -> Module
protoModule result pri@(ProtoInfo protoName _ _ keyInfos _ _ _) fdpBS
  = let protoName = protoMod pri
        (extendees,myKeys) = unzip $ F.toList (extensionKeys pri)
        m = ModuleName (fqMod protoName)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
        exportNames = map (EVar . UnQual . Ident) ["protoInfo","fileDescriptorProto"]
        imports = (protoImports ++) . mergeImports $
                    mapMaybe (importPN result m Normal) $
                      extendees ++ mapMaybe typeName myKeys
    in Module src m [] Nothing (Just (exportKeys++exportNames)) imports
         (keysXTypeVal protoName keyInfos ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
 where protoImports = standardImports False (not . Seq.null . extensionKeys $ pri) ++
         [ ImportDecl src (ModuleName "Text.DescriptorProtos.FileDescriptorProto") False False Nothing
                        (Just (False,[IAbs (Ident "FileDescriptorProto")]))
         , ImportDecl src (ModuleName "Text.ProtocolBuffers.Reflections") False False Nothing
                        (Just (False,[IAbs (Ident "ProtoInfo")]))
         , ImportDecl src (ModuleName "Text.ProtocolBuffers.WireMessage") True False (Just (ModuleName "P'"))
                        (Just (False,[IVar (Ident "wireGet,getFromBS")])) ]

{-
protoModule :: Result -> ProtoInfo -> ByteString -> Module -- XXX rec
protoModule rec pri@(ProtoInfo protoName _ _ keyInfos _ _ _) fdpBS
  = let exportKeys = map (EVar . unqualFName . fieldName . snd) (F.toList keyInfos)
        exportNames = map (EVar . UnQual . Ident) ["protoInfo","fileDescriptorProto"]
        imports = protoImports ++ map formatImport (protoImport pri)
    in Module src (ModuleName (fqMod protoName)) [] Nothing (Just (exportKeys++exportNames))
              imports (keysXTypeVal protoName keyInfos ++ embed'ProtoInfo pri ++ embed'fdpBS fdpBS)
  where protoImports = standardImports False (not . Seq.null . extensionKeys $ pri) ++
                       [ ImportDecl src (ModuleName "Text.DescriptorProtos.FileDescriptorProto") False False Nothing
                           (Just (False,[IAbs (Ident "FileDescriptorProto")]))
                       , ImportDecl src (ModuleName "Text.ProtocolBuffers.Reflections") False False Nothing
                           (Just (False,[IAbs (Ident "ProtoInfo")]))
                       , ImportDecl src (ModuleName "Text.ProtocolBuffers.WireMessage") True False (Just (ModuleName "P'"))
                           (Just (False,[IVar (Ident "wireGet,getFromBS")]))
                       ]
        formatImport ((a,b),s) = ImportDecl src (ModuleName a) True False asM (Just (False,map (IAbs . Ident) (S.toList s)))
          where asM | a==b = Nothing
                    | otherwise = Just (ModuleName b)

protoImport :: ProtoInfo -> [((String,String),S.Set String)]
protoImport protoInfo
    = M.assocs . M.fromListWith S.union . filter isForeign . map withMod $ keyNames
  where isForeign = let here = fqMod protoName
                    in (\ ((a,_),_) -> a/=here)
        protoName = protoMod protoInfo
        withMod p@(ProtoName _ _prefix modname base) = ((fqMod p,joinMod modname),S.singleton (mName base))
        keyNames = F.foldr (\ (e,fi) rest -> e : addName fi rest) [] (extensionKeys protoInfo)
        addName fi rest = maybe rest (:rest) (typeName fi)
-}
embed'ProtoInfo :: ProtoInfo -> [Decl]
embed'ProtoInfo pri = [ myType, myValue ]
  where myType = TypeSig src [ Ident "protoInfo" ] (TyCon (local "ProtoInfo"))
        myValue = PatBind src (PApp (local "protoInfo") []) (UnGuardedRhs $
                    pvar "read" $$ litStr (show pri)) noWhere

embed'fdpBS :: ByteString -> [Decl]
embed'fdpBS bs = [ myType, myValue ]
  where myType = TypeSig src [ Ident "fileDescriptorProto" ] (TyCon (local "FileDescriptorProto"))
        myValue = PatBind src (PApp (local "fileDescriptorProto") []) (UnGuardedRhs $
                    pvar "getFromBS" $$
                      Paren (pvar "wireGet" $$ litInt 11) $$ 
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
        classes = ["Show","Eq","Ord","Typeable","Mergeable","Default","Wire","GPB","ReflectDescriptor"]
                  ++ if hasExt di then ["ExtendMessage"] else []
                  ++ if storeUnknown di then ["UnknownMessage"] else []
        instMesAPI = InstDecl src [] (private "MessageAPI")
                       [TyVar (Ident "msg'"), TyFun (TyVar (Ident "msg'")) (TyCon un),  (TyCon un)] []
        dataDecl = DataDecl src DataType [] (baseIdent protoName) [] [] []
        mkInst s = InstDecl src [] (private s) [TyCon un] []
    in Module src (ModuleName (fqMod protoName)) [] Nothing (Just [EAbs un]) minimalImports
         (dataDecl : instMesAPI : map mkInst classes)

-- This builds on the output of descriptorBootModule and declares a hs-boot that
-- declares the data type and the keys
descriptorKeyBootModule :: Result -> DescriptorInfo -> Module
descriptorKeyBootModule result di
  = let Module p1 m@(ModuleName self) p3 p4 (Just exports) imports decls = descriptorBootModule di
        (extendees,myKeys) = unzip $ F.toList (keys di)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
        importTypes = mergeImports . mapMaybe (importPN result m Source) . nubSort $
                        extendees ++ mapMaybe typeName myKeys
        declKeys = keysXType (descName di) (keys di)
    in Module p1 m p3 p4 (Just (exports++exportKeys)) (imports++importTypes) (decls++declKeys)

-- This build the 'Key module that defines the keys only
descriptorKeyfileModule :: Result -> DescriptorInfo -> Module
descriptorKeyfileModule result di
  = let protoName'Key = (descName di) { baseName = MName . (++"'Key") . mName $ (baseName (descName di)) }
        (extendees,myKeys) = unzip $ F.toList (keys di)
        m = ModuleName (fqMod protoName'Key)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
        importTypes = mergeImports . mapMaybe (importPN result m KeyFile) . nubSort $
                        extendees ++ mapMaybe typeName myKeys
        declKeys = keysXTypeVal protoName'Key (keys di)
    in Module src m [] Nothing (Just exportKeys) (minimalImports++importTypes) declKeys

-- This builds the normal module
descriptorNormalModule :: Result -> DescriptorInfo -> Module
descriptorNormalModule result di
  = let protoName = descName di
        un = unqualName protoName
        myKind = getKind result (pKey protoName)
        sepKey = myKind == SplitKeyTypeBoot
        (extendees,myKeys) = unzip $ F.toList (keys di)
        m = ModuleName (fqMod protoName)
        exportKeys = map (EVar . unqualFName . fieldName) myKeys
        imports = (standardImports False (hasExt di) ++) . mergeImports . concat $
                    [ mapMaybe (importPN result m Normal) $
                       extendees ++ mapMaybe typeName (myKeys ++ (F.toList (fields di)))
                    , mapMaybe (importPFN result m) (map fieldName (myKeys ++ F.toList (knownKeys di))) ]
        declKeys | sepKey = []
                 | otherwise = keysXTypeVal (descName di) (keys di)
    in Module src m [] Nothing (Just (EThingAll un : exportKeys)) imports
         (descriptorX di : declKeys ++ instancesDescriptor di)

minimalImports :: [ImportDecl]
minimalImports =
  [ ImportDecl src (ModuleName "Prelude") True False (Just (ModuleName "P'")) Nothing
  , ImportDecl src (ModuleName "Text.ProtocolBuffers.Header") True False (Just (ModuleName "P'")) Nothing ]

standardImports :: Bool -> Bool -> [ImportDecl]
standardImports isEnumMod ext =
  [ ImportDecl src (ModuleName "Prelude") False False Nothing (Just (False,ops))
  , ImportDecl src (ModuleName "Prelude") True False (Just (ModuleName "P'")) Nothing
  , ImportDecl src (ModuleName "Text.ProtocolBuffers.Header") True False (Just (ModuleName "P'")) Nothing ]
 where ops | ext = map (IVar . Symbol) $ base ++ ["==","<=","&&"," || "]
           | otherwise = map (IVar . Symbol) base
       base | isEnumMod = ["+","."]
            | otherwise = ["+"]

keysXType :: ProtoName -> Seq KeyInfo -> [Decl]
keysXType self ks = map (makeKeyType self) . F.toList $ ks

keysXTypeVal :: ProtoName -> Seq KeyInfo -> [Decl]
keysXTypeVal self ks = concatMap (\ki -> [makeKeyType self ki,makeKeyVal self ki]) . F.toList $ ks

makeKeyType :: ProtoName -> KeyInfo -> Decl
makeKeyType self (extendee,f) = keyType
  where keyType = TypeSig src [ baseIdent' . fieldName $ f ] (foldl1 TyApp . map TyCon $
                    [ private "Key", private labeled
                    , if extendee /= self then qualName extendee else unqualName extendee
                    , typeQName ])
        labeled | canRepeat f = "Seq"
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
makeKeyVal self (extendee,f) = keyVal
  where typeNumber = getFieldType . typeCode $ f
        keyVal = PatBind src (PApp (unqualFName . fieldName $ f) []) (UnGuardedRhs
                   (pvar "Key" $$ litInt (getFieldId (fieldNumber f))
                               $$ litInt typeNumber
                               $$ maybe (pvar "Nothing")
                                        (Paren . (pvar "Just" $$) . (defToSyntax (typeCode f)))
                                        (hsDefault f)
                   )) noWhere

defToSyntax :: FieldType -> HsDefault -> Exp
defToSyntax tc x =
  case x of
    HsDef'Bool b -> pcon (show b)
    HsDef'ByteString bs -> (if tc == 9 then (\xx -> Paren (pvar "Utf8" $$ xx)) else id) $
                           (Paren $ pvar "pack" $$ litStr (LC.unpack bs))
    HsDef'Rational r | r < 0 -> Paren $ Lit (Frac r)
                     | otherwise -> Lit (Frac r)
    HsDef'Integer i -> litInt i 
    HsDef'Enum s -> Paren $ pvar "read" $$ litStr s

{-
-- Imports needed by 'Key modules
keyImport :: DescriptorInfo -> [((String,String),S.Set String)]
keyImport di = M.assocs . M.fromListWith S.union . map withMod $ allNames
 where protoName = descName di
       withMod (Left p@(ProtoName _ _prefix modname base)) = ((fqMod p,joinMod modname),S.singleton (mName base))
       withMod (Right (ProtoFName _ prefix modname base)) = ((joinMod (prefix++modname),joinMod modname),S.singleton (fName base))
       allNames = F.foldr addName keyNames (fields di)
       keyNames = F.foldr (\ (e,fi) rest -> Left e : addName fi rest) [] (keys di)
       addName fi rest = maybe rest (:rest) (fmap Left (typeName fi))

descriptorModule :: Result -> DescriptorInfo -> Module -- XXX result
descriptorModule result di
  = let protoName = descName di
        un = unqualName protoName
        imports = standardImports False (hasExt di) ++ map formatImport (toImport di)
        exportKeys = map (EVar . unqualFName . fieldName . snd) (F.toList (keys di))
        formatImport ((a,b),s) = ImportDecl src (ModuleName a) True False asM
                                   (Just (False, map (IAbs . Ident) (S.toList s)))
          where asM | a==b = Nothing
                    | otherwise = Just (ModuleName b)
    in Module src (ModuleName (fqMod protoName)) [] Nothing
         (Just (EThingAll un : exportKeys))
         imports (descriptorX di : (keysX protoName (keys di) ++ instancesDescriptor di))

toImport :: DescriptorInfo -> [((String,String),S.Set String)]
toImport di
    = M.assocs . M.fromListWith S.union . filter isForeign . map withMod $ allNames
  where isForeign = let here = fqMod protoName
                    in (\ ((a,_),_) -> a/=here)
        protoName = descName di
        withMod (Left p@(ProtoName _ _prefix modname base)) = ((fqMod p,joinMod modname),S.singleton (mName base))
        withMod (Right (ProtoFName _ prefix modname base)) = ((joinMod (prefix++modname),joinMod modname),S.singleton (fName base))
        allNames = F.foldr addName keyNames (fields di)
        keyNames = F.foldr (\ (e,fi) rest -> Left e : addName fi rest) keysKnown (keys di)
        addName fi rest = maybe rest (:rest) (fmap Left (typeName fi))
        keysKnown = F.foldr (\fi rest -> Right (fieldName fi) : rest) [] (knownKeys di)
-}

{-
keysX :: ProtoName -> Seq KeyInfo -> [Decl]
keysX self i = concatMap (makeKey self) . F.toList $ i

makeKey :: ProtoName -> KeyInfo -> [Decl]
makeKey self (extendee,f) = [ keyType, keyVal ]
  where keyType = TypeSig src [ baseIdent' . fieldName $ f ] (foldl1 TyApp . map TyCon $
                    [ private "Key", private labeled
                    , if extendee /= self then qualName extendee else unqualName extendee
                    , typeQName ])
        labeled | canRepeat f = "Seq"
                | otherwise = "Maybe"
        typeNumber = getFieldType . typeCode $ f
        typeQName :: QName
        typeQName = case useType typeNumber of
                      Just s -> private s
                      Nothing -> case typeName f of
                                   Just s | self /= s -> qualName s
                                          | otherwise -> unqualName s
                                   Nothing -> error $  "No Name for Field!\n" ++ show f
        keyVal = PatBind src (PApp (unqualFName . fieldName $ f) []) (UnGuardedRhs
                   (pvar "Key" $$ litInt (getFieldId (fieldNumber f))
                               $$ litInt typeNumber
                               $$ maybe (pvar "Nothing")
                                        (Paren . (pvar "Just" $$) . (defToSyntax (typeCode f)))
                                        (hsDefault f)
                   )) noWhere
-}

descriptorX :: DescriptorInfo -> Decl
descriptorX di = DataDecl src DataType [] name [] [QualConDecl src [] [] con] derives
  where self = descName di
        name = baseIdent self
        con = RecDecl name eFields
                where eFields = F.foldr ((:) . fieldX) end (fields di)
                      end = (if hasExt di then (extfield:) else id) 
                          $ (if storeUnknown di then [unknownField] else [])
        extfield :: ([Name],BangType)
        extfield = ([Ident "ext'field"],UnBangedTy (TyCon (private "ExtField")))
        unknownField :: ([Name],BangType)
        unknownField = ([Ident "unknown'field"],UnBangedTy (TyCon (private  "UnknownField")))
        fieldX :: FieldInfo -> ([Name],BangType)
        fieldX fi = ([baseIdent' . fieldName $ fi],UnBangedTy (labeled (TyCon typed)))
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
   ]

instanceExtendMessage :: DescriptorInfo -> Decl
instanceExtendMessage di
    = InstDecl src [] (private "ExtendMessage") [TyCon (unqualName (descName di))]
        [ inst "getExtField" [] (lvar "ext'field")
        , inst "putExtField" [patvar "e'f", patvar "msg"] putextfield
        , inst "validExtRanges" [patvar "msg"] (pvar "extRanges" $$ (Paren $ pvar "reflectDescriptorInfo" $$ lvar "msg"))
        ]
  where putextfield = RecUpdate (lvar "msg") [ FieldUpdate (local "ext'field") (lvar "e'f") ]

instanceUnknownMessage :: DescriptorInfo -> Decl
instanceUnknownMessage di
    = InstDecl src [] (private "UnknownMessage") [TyCon (unqualName (descName di))]
        [ inst "getUnknownField" [] (lvar "unknown'field")
        , inst "putUnknownField" [patvar "u'f",patvar "msg"] putunknownfield
        ]
  where putunknownfield = RecUpdate (lvar "msg") [ FieldUpdate (local "unknown'field") (lvar "u'f") ]

instanceMergeable :: DescriptorInfo -> Decl
instanceMergeable di
    = InstDecl src [] (private "Mergeable") [TyCon un]
        [ inst "mergeEmpty" [] (foldl' App (Con un) (replicate len (pcon "mergeEmpty")))
        , inst "mergeAppend" [PApp un patternVars1, PApp un patternVars2]
                             (foldl' App (Con un) (zipWith append vars1 vars2))
        ]
  where un = unqualName (descName di)
        len = (if hasExt di then succ else id)
            $ (if storeUnknown di then succ else id)
            $ Seq.length (fields di)
        patternVars1,patternVars2 :: [Pat]
        patternVars1 = take len inf
            where inf = map (\n -> patvar ("x'" ++ show n)) [1..]
        patternVars2 = take len inf
            where inf = map (\n -> patvar ("y'" ++ show n)) [1..]
        vars1,vars2 :: [Exp]
        vars1 = take len inf
            where inf = map (\n -> lvar ("x'" ++ show n)) [1..]
        vars2 = take len inf
            where inf = map (\n -> lvar ("y'" ++ show n)) [1..]
        append x y = Paren $ pvar "mergeAppend" $$ x $$ y

instanceDefault :: DescriptorInfo -> Decl
instanceDefault di
    = InstDecl src [] (private "Default") [TyCon un]
        [ inst "defaultValue" [] (foldl' App (Con un) deflistExt) ]
  where un = unqualName (descName di)
        deflistExt = F.foldr ((:) . defX) end (fields di)
        end = (if hasExt di then (pvar "defaultValue":) else id) 
            $ (if storeUnknown di then [pvar "defaultValue"] else [])

        defX :: FieldInfo -> Exp
        defX fi | isRequired fi = dv1
                | otherwise = dv2
          where dv1 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> defToSyntax (typeCode fi) hsdef
                dv2 = case hsDefault fi of
                        Nothing -> pvar "defaultValue"
                        Just hsdef -> Paren $ pcon "Just" $$ defToSyntax (typeCode fi) hsdef

instanceMessageAPI :: ProtoName -> Decl
instanceMessageAPI protoName
    = InstDecl src [] (private "MessageAPI")
        [TyVar (Ident "msg'"), TyFun (TyVar (Ident "msg'")) (TyCon un),  (TyCon un)]
        [ inst "getVal" [patvar "m'",patvar "f'"] (App (lvar "f'" ) (lvar "m'")) ]
  where un = unqualName protoName

instanceWireDescriptor :: DescriptorInfo -> Decl
instanceWireDescriptor di@(DescriptorInfo { descName = protoName
                                          , fields = fieldInfos
                                          , extRanges = allowedExts
                                          , knownKeys = fieldExts })
  = let me = unqualName protoName
        extensible = not (null allowedExts)
        len = (if extensible then succ else id) 
            $ (if storeUnknown di then succ else id)
            $ Seq.length fieldInfos
        mine = PApp me . take len . map (\n -> patvar ("x'" ++ show n)) $ [1..]
        vars = take len . map (\n -> lvar ("x'" ++ show n)) $ [1..]
        mExt | extensible = Just (vars !! Seq.length fieldInfos)
             | otherwise = Nothing
        mUnknown | storeUnknown di = Just (last vars)
                 | otherwise = Nothing

        -- first case is for Group behavior, second case is for Message behavior, last is error handler
        cases g m e = Case (lvar "ft'") [ Alt src (litIntP 10) (UnGuardedAlt g) noWhere
                                          , Alt src (litIntP 11) (UnGuardedAlt m) noWhere
                                          , Alt src PWildCard  (UnGuardedAlt e) noWhere
                                          ]

        sizeCases = UnGuardedRhs $ cases (lvar "calc'Size") 
                                           (pvar "prependMessageSize" $$ lvar "calc'Size")
                                           (pvar "wireSizeErr" $$ lvar "ft'" $$ lvar "self'")
        whereCalcSize = BDecls [defun "calc'Size" [] sizes]
        sizes | null sizesList = Lit (Hse.Int 0)
              | otherwise = Paren (foldl1' (+!) sizesList)
          where (+!) = mkOp "+"
                sizesList | Just v <- mUnknown = sizesListExt ++ [ pvar "wireSizeUnknownField" $$ v ]
                          | otherwise = sizesListExt
                sizesListExt | Just v <- mExt = sizesListFields ++ [ pvar "wireSizeExtField" $$ v ] 
                             | otherwise = sizesListFields
                sizesListFields =  zipWith toSize vars . F.toList $ fieldInfos
        toSize var fi = let f = if isRequired fi then "wireSizeReq"
                                  else if canRepeat fi then "wireSizeRep"
                                      else "wireSizeOpt"
                        in foldl' App (pvar f) [ litInt (wireTagLength fi)
                                                 , litInt (getFieldType (typeCode fi))
                                                 , var]

        putCases = UnGuardedRhs $ cases
          (lvar "put'Fields")
          (Do [ Qualifier $ pvar "putSize" $$
                    (Paren $ foldl' App (pvar "wireSize") [ litInt 10 , lvar "self'" ])
                , Qualifier $ lvar "put'Fields" ])
          (pvar "wirePutErr" $$ lvar "ft'" $$ lvar "self'")
        wherePutFields = BDecls [defun "put'Fields" [] (Do putStmts)]
        putStmts = putStmtsContent
          where putStmtsContent | null putStmtsAll = [Qualifier $ pvar "return" $$ Con (Special UnitCon)]
                                | otherwise = putStmtsAll
                putStmtsAll | Just v <- mUnknown = putStmtsListExt ++ [ Qualifier $ pvar "wirePutUnknownField" $$ v ]
                             | otherwise = putStmtsListExt
                putStmtsListExt | Just v <- mExt = sortedPutStmtsList ++ [ Qualifier $ pvar "wirePutExtField" $$ v ]
                                | otherwise = sortedPutStmtsList
                sortedPutStmtsList = map snd                                          -- remove number
                                     . sortBy (compare `on` fst)                      -- sort by number
                                     . zip (map fieldNumber . F.toList $ fieldInfos)  -- add number as fst
                                     $ putStmtsList
                putStmtsList = zipWith toPut vars . F.toList $ fieldInfos
        toPut var fi = let f = if isRequired fi then "wirePutReq"
                                 else if canRepeat fi then "wirePutRep"
                                     else "wirePutOpt"
                       in Qualifier $
                          foldl' App (pvar f) [ litInt (getWireTag (wireTag fi))
                                                , litInt (getFieldType (typeCode fi))
                                                , var]

        getCases = UnGuardedRhs $ cases
          (pvar "getBareMessageWith" $$ lvar "check'allowed")
          (pvar "getMessageWith" $$ lvar "check'allowed")
          (pvar "wireGetErr" $$ lvar "ft'")
        whereDecls = BDecls [whereUpdateSelf,whereAllowed,whereCheckAllowed]
        whereAllowed = defun "allowed'wire'Tags" [] (pvar "fromDistinctAscList" $$ List (map litInt allowed))
        allowed = sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di)] ++
                         [ getWireTag (wireTag f) | f <- F.toList (knownKeys di)]
        locals = ["wire'Tag","field'Number","wire'Type","old'Self"]
        whereCheckAllowed = defun "check'allowed" (map patvar locals) process
         where process = if storeUnknown di then catchUn updateBranch else updateBranch
               catchUn s = pvar "catchError" $$ Paren s
                 $$ Paren (Lambda src [PWildCard] (args (pvar "loadUnknown")))
               updateBranch | null allowed = extBranch
                            | otherwise = If (pvar "member" $$ lvar "wire'Tag" $$ lvar "allowed'wire'Tags")
                                               (lvar "update'Self" $$ lvar "field'Number" $$ lvar "old'Self")
                                               extBranch
               extBranch | extensible = If (isAllowedExt (lvar "field'Number"))
                                             (args (pvar "loadExtension"))
                                             unknownBranch
                         | otherwise = unknownBranch
               unknownBranch =args (pvar "unknown")
               args x = x $$ lvar "field'Number" $$ lvar "wire'Type" $$ lvar "old'Self"
        isAllowedExt x = pvar "or" $$ List ranges where
          (<=!) = mkOp "<="; (&&!) = mkOp "&&"; (==!) = mkOp ("==")
          ranges = map (\ (FieldId lo,FieldId hi) -> if hi < maxHi
                                                       then if lo == hi
                                                              then (x ==! litInt lo)
                                                              else (litInt lo <=! x) &&! (x <=! litInt hi)
                                                       else litInt lo <=! x) allowedExts
             where FieldId maxHi = maxBound
        whereUpdateSelf = defun "update'Self" [patvar "field'Number", patvar "old'Self"]
                            (Case (lvar "field'Number") updateAlts)
        updateAlts = map toUpdate (F.toList fieldInfos)
                     ++ (if extensible && (not (Seq.null fieldExts)) then map toUpdateExt (F.toList fieldExts) else [])
                     ++ [Alt src PWildCard (UnGuardedAlt $
                           pvar "unknownField" $$ (lvar "old'Self") $$ (lvar "field'Number")) noWhere]
        toUpdateExt fi = Alt src (litIntP . getFieldId . fieldNumber $ fi) (UnGuardedAlt $
                           pvar "wireGetKey" $$ Var (mayQualName protoName (fieldName fi)) $$ lvar "old'Self") noWhere
        toUpdate fi = Alt src (litIntP . getFieldId . fieldNumber $ fi) (UnGuardedAlt $ 
                        pvar "fmap" $$ (Paren $ Lambda src [patvar "new'Field"] $
                                          RecUpdate (lvar "old'Self")
                                                    [FieldUpdate (unqualFName . fieldName $ fi)
                                                                 (labelUpdate fi)])
                                    $$ (Paren (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ Paren ((Var . unqualFName . fieldName $ fi)
                                                                  $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (pcon "Just" $$ lvar "new'Field")
            where qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` [10,11] =
                               pvar "mergeAppend" $$ Paren ( (Var . unqualFName . fieldName $ fi)
                                                               $$ lvar "old'Self" )
                                                  $$ Paren x
                           | otherwise = x
        -- in the above, the [10,11] check optimizes using the
        -- knowledge that only TYPE_MESSAGE and TYPE_GROUP have merges
        -- that are not right-biased replacements.  The "append" uses
        -- knowledge of how all repeated fields get merged.
    in InstDecl src [] (private "Wire") [TyCon me] . map InsDecl $
        [ FunBind [Match src (Ident "wireSize") [patvar "ft'",PAsPat (Ident "self'") (PParen mine)] sizeCases whereCalcSize]
        , FunBind [Match src (Ident "wirePut")  [patvar "ft'",PAsPat (Ident "self'") (PParen mine)] putCases wherePutFields]
        , FunBind [Match src (Ident "wireGet") [patvar "ft'"] getCases whereDecls]
        ]

instanceReflectDescriptor :: DescriptorInfo -> Decl
instanceReflectDescriptor di
    = InstDecl src [] (private "ReflectDescriptor") [TyCon (unqualName (descName di))]
        [ inst "getMessageInfo" [PWildCard] gmi
        , inst "reflectDescriptorInfo" [ PWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: Exp
        rdi = pvar "read" $$ litStr (show di)
        gmi,reqId,allId :: Exp
        gmi = pcon "GetMessageInfo" $$ Paren reqId $$ Paren allId
        reqId = pvar "fromDistinctAscList" $$
                List (map litInt . sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di), isRequired f])
        allId = pvar "fromDistinctAscList" $$
                List (map litInt . sort $ [ getWireTag (wireTag f) | f <- F.toList (fields di)] ++
                                            [ getWireTag (wireTag f) | f <- F.toList (knownKeys di)])

------------------------------------------------------------------

derives,derivesEnum :: [QName]
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
useType  x = imp $ "useType: Unknown type code (expected 1 to 18) of "++show x
