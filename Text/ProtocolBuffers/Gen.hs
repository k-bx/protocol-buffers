-- try "testD" and "testLabel"
--
-- Turn *Proto into Language.Haskell.Exts.Syntax from haskell-src-exts package
-- Need to get this just far enough to allow bootstrapping of 'descriptor.proto'
-- Done: Enum modules
-- Done: Descriptor modules (except for default values and reflection)
--
-- Note that this must eventually also generate hs-boot files to allow
-- for breaking mutual recursion.  This is ignored for getting
-- descriptor.proto running.
--
-- Mangling: For the current moment, assume the mangling is done in a prior pass:
--   (*) Uppercase all module names and type names
--   (*) lowercase all field names
--   (*) add a prime after all field names than conflict with reserved words
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
-- The above fails to mentions enums' default is their first value (always exists)
-- see something like http://msdn.microsoft.com/en-us/library/6aw8xdf2.aspx for c escape parsing
-- So rendering a message to text is ugly, the strings are valid UTF-8 and so are rendered in
-- some other unicode form, while bytes must be unparsed to use. The only sane thing to do is
-- put a parsed and converted Haskell data type into the defaults interface.
-- This has been done in Reflections.hs
module Text.ProtocolBuffers.Gen where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
{-
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto(ExtensionRange)
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto.ExtensionRange(ExtensionRange(..))
-}
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
{- -- unused or unusable
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions(CType)
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions.CType(CType(..))
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions(OptimizeMode)
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions.OptimizeMode(OptimizeMode(..))
-}
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
{-  -- related to the rpc system
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions(MessageOptions(..))
import qualified Text.DescriptorProtos.MethodOptions                  as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions                  as D.MethodOptions(MethodOptions(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto) 
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..)) 
import qualified Text.DescriptorProtos.ServiceOptions                 as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions                 as D.ServiceOptions(ServiceOptions(..))
-}

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Reflections

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.Maybe(fromMaybe,catMaybes)
import Data.List(sort,group,foldl)
import Data.Sequence(Seq)
import qualified Data.Sequence as S
import Data.Foldable as F
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

-- -- -- -- Helper functions

dotPre :: String -> String -> String
dotPre "" = id
dotPre s | '.' == last s = (s ++)
         | otherwise = (s ++) . ('.':)

-- Take a bytestring of "A" into "Right A" and "A.B.C" into "Left (A.B,C)"
splitMod :: ByteString -> Either (ByteString,ByteString) ByteString
splitMod bs = case BSC.spanEnd ('.'/=) bs of
                (pre,post) | BSC.length pre <= 1 -> Right bs
                           | otherwise -> Left (BSC.init pre,post)

unqual :: ByteString -> HsQName
unqual bs = UnQual (base bs)

base :: ByteString -> HsName
base bs = case splitMod bs of
            Right typeName -> (ident typeName)
            Left (_,typeName) -> (ident typeName)

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

ident :: ByteString -> HsName
ident bs = HsIdent (U.toString bs)

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (private s))

-- 'qual' and 'qmodname' are only correct for simple or fully looked-up names.
qual :: ByteString -> HsQName
qual bs = case splitMod bs of
            Right typeName -> UnQual (ident typeName)
            Left (modName,typeName) -> Qual (Module (U.toString modName)) (ident typeName)

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)

--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------

testLabel = putStrLn . prettyPrint $ enumModule "Text" e

e :: D.EnumDescriptorProto.EnumDescriptorProto
e = defaultValue
    { D.EnumDescriptorProto.name = Just (BSC.pack "DescriptorProtos.FieldDescriptorProto.Label")
    , D.EnumDescriptorProto.value = S.fromList
      [ defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_OPTIONAL")
                     , D.EnumValueDescriptorProto.number = Just 1 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_REQUIRED")
                     , D.EnumValueDescriptorProto.number = Just 2 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_REPEATED")
                     , D.EnumValueDescriptorProto.number = Just 3 }
      ]
    }

fqName :: ProtoName -> String
fqName (ProtoName a b c) = dotPre a (dotPre b c)

enumModule :: String -> D.EnumDescriptorProto -> HsModule
enumModule prefix
           e@(D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just rawName})
    = let protoName = case splitMod rawName of
                        Left (m,b) -> ProtoName prefix (U.toString m) (U.toString b)
                        Right b    -> ProtoName prefix ""             (U.toString b)
      in HsModule src
             (Module (fqName protoName))
             (Just [HsEThingAll (UnQual (HsIdent (baseName protoName)))])
             standardImports
             (enumDecls protoName e)
  
enumValues :: D.EnumDescriptorProto -> [(Integer,HsName)]
enumValues (D.EnumDescriptorProto.EnumDescriptorProto
            { D.EnumDescriptorProto.value = value}) 
    = sort $ F.foldr ((:) . oneValue) [] value
  where oneValue  :: D.EnumValueDescriptorProto -> (Integer,HsName)
        oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                  { D.EnumValueDescriptorProto.name = Just name
                  , D.EnumValueDescriptorProto.number = Just number })
            = (toInteger number,ident name)
      
enumValueX :: (Integer,HsName) -> HsQualConDecl
enumValueX (_,hsName) = HsQualConDecl src [] [] (HsConDecl hsName [])

enumX :: D.EnumDescriptorProto -> HsDecl
enumX e@(D.EnumDescriptorProto.EnumDescriptorProto
         { D.EnumDescriptorProto.name = Just rawName})
    = HsDataDecl src DataType [] (base rawName) [] (map enumValueX values) derives
        where values = enumValues e


enumDecls :: ProtoName -> D.EnumDescriptorProto -> [HsDecl]
enumDecls p e = enumX e : Prelude.concat [ instanceMergeableEnum e
                                         , [ instanceBounded e
                                           , instanceDefaultEnum e
                                           , instanceEnum e
                                           , instanceReflectEnum p e ] ]


instanceBounded :: D.EnumDescriptorProto -> HsDecl
instanceBounded e@(D.EnumDescriptorProto.EnumDescriptorProto
                   { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Bounded") [HsTyCon (unqual name)] 
                 (map (HsInsDecl . HsFunBind) [set "minBound" (head values)
                                              ,set "maxBound" (last values)]) where
        values = enumValues e
        set f (_,n) = [HsMatch src (HsIdent f) [] (HsUnGuardedRhs (HsCon (UnQual n))) noWhere]

instanceEnum :: D.EnumDescriptorProto -> HsDecl
instanceEnum e@(D.EnumDescriptorProto.EnumDescriptorProto
                { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Enum") [HsTyCon (unqual name)] 
                 (map (HsInsDecl . HsFunBind) [fromEnum',toEnum',succ',pred']) where
        values = enumValues e
        fromEnum' = map fromEnum'one values
        fromEnum'one (v,n) = HsMatch src (HsIdent "fromEnum") [HsPApp (UnQual n) []]
                               (HsUnGuardedRhs (HsLit (HsInt v))) noWhere
        toEnum' = map toEnum'one values
        toEnum'one (v,n) = HsMatch src (HsIdent "toEnum") [HsPLit (HsInt v)]
                               (HsUnGuardedRhs (HsCon (UnQual n))) noWhere
        succ' = zipWith (equate "succ") values (tail values)
        pred' = zipWith (equate "pred") (tail values) values
        equate f (_,n1) (_,n2) = HsMatch src (HsIdent f) [HsPApp (UnQual n1) []]
                                   (HsUnGuardedRhs (HsCon (UnQual n2))) noWhere

hsZero :: HsExp
hsZero = HsLit (HsInt 0)

hsEmpty :: HsExp
hsEmpty = HsVar (private "emptyBS")

hsMinBound :: HsExp
hsMinBound = HsVar (private "minBound")

instanceMergeableEnum :: D.EnumDescriptorProto -> [HsDecl]
instanceMergeableEnum (D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just name }) =
    [ HsInstDecl src [] (private "Mergeable") [HsTyCon (unqual name)] []
    , HsInstDecl src [] (private "Mergeable") [typeApp "Maybe" $ HsTyCon (unqual name)]
      [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeEmpty") [] 
                                          (HsUnGuardedRhs (HsCon (private "Nothing"))) noWhere])
      , HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeAppend") [] 
                                          (HsUnGuardedRhs (HsVar (private "mayMerge"))) noWhere])
      ]
    ]

{- from google's descriptor.h, about line 346:

  // Get the field default value if cpp_type() == CPPTYPE_ENUM.  If no
  // explicit default was defined, the default is the first value defined
  // in the enum type (all enum types are required to have at least one value).
  // This never returns NULL.

-}

instanceDefaultEnum :: D.EnumDescriptorProto -> HsDecl
instanceDefaultEnum (D.EnumDescriptorProto.EnumDescriptorProto
                     { D.EnumDescriptorProto.name = Just name })
    = HsInstDecl src [] (private "Default") [HsTyCon (unqual name)]
      [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "defaultValue") [] 
                                          (HsUnGuardedRhs hsMinBound) noWhere])
      ]

instanceReflectEnum :: ProtoName -> D.EnumDescriptorProto -> HsDecl
instanceReflectEnum protoName@(ProtoName a b c)
                    (D.EnumDescriptorProto.EnumDescriptorProto
                     { D.EnumDescriptorProto.name = Just rawName })
    = HsInstDecl src [] (private "ReflectEnum") [HsTyCon (unqual rawName)]
      [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "reflectEnum") [] 
                                          (HsUnGuardedRhs ascList) noWhere])
      , HsInsDecl (HsFunBind [HsMatch src (HsIdent "reflectEnumInfo") [ HsPWildCard ] 
                                          (HsUnGuardedRhs ei) noWhere])
      ]
  where values = enumValues e
        ascList,ei,protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns),HsCon (UnQual n)]
        ei = Prelude.foldl HsApp (HsCon (private "EnumInfo")) [protoNameExp,HsList (map two values)]
          where two (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns)]
        protoNameExp = HsParen $ Prelude.foldl HsApp (HsCon (private "ProtoName")) [HsLit (HsString a)
                                                                                   ,HsLit (HsString b)
                                                                                   ,HsLit (HsString c)]

--------------------------------------------
-- DescriptorProto module creation is unfinished
--   There are difficult namespace issues
--------------------------------------------

descriptorModule :: String -> D.DescriptorProto -> HsModule
descriptorModule prefix
                 d@(D.DescriptorProto.DescriptorProto
                    { D.DescriptorProto.name = Just rawName
                    , D.DescriptorProto.field = field })
    = let self = UnQual . HsIdent . U.toString . either snd id . splitMod $ rawName
          fqModuleName = Module (dotPre prefix (U.toString rawName))
          imports = standardImports ++ map formatImport (toImport d)
      in HsModule src fqModuleName (Just [HsEThingAll self]) imports (descriptorX d : instancesDescriptor d)
  where formatImport (Left (m,t)) = HsImportDecl src (Module (dotPre prefix (dotPre m t))) True
                                      (Just (Module m)) (Just (False,[HsIAbs (HsIdent t)]))
        formatImport (Right t)    = HsImportDecl src (Module (dotPre prefix t)) False
                                      Nothing (Just (False,[HsIAbs (HsIdent t)]))

standardImports = [ HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
                  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]

-- Create a list of (Module,Name) to import
-- Assumes that all self references are _not_ qualified!
toImport :: D.DescriptorProto -> [Either (String,String) String]
toImport (D.DescriptorProto.DescriptorProto
          { D.DescriptorProto.name = Just name
          , D.DescriptorProto.field = field })
    = map head . group . sort
      . map (either (\(m,t) -> Left (U.toString m,U.toString t)) (Right . U.toString))
      . map splitMod
      . filter (selfName /=)
      . catMaybes 
      . F.foldr ((:) . mayImport) [] $
        field
  where selfName = either snd id (splitMod name)
        mayImport (D.FieldDescriptorProto.FieldDescriptorProto
                   { D.FieldDescriptorProto.type' = type'
                   , D.FieldDescriptorProto.type_name = type_name })
            = answer
          where answer     = maybe answerName checkType type'
                checkType  = maybe answerName (const Nothing) . useType
                answerName = maybe (error "No Name for Type!") Just type_name

-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
fieldX :: D.FieldDescriptorProto -> ([HsName],HsBangType)
fieldX (D.FieldDescriptorProto.FieldDescriptorProto
         { D.FieldDescriptorProto.name = Just name
         , D.FieldDescriptorProto.label = Just labelEnum
         , D.FieldDescriptorProto.type' = type'
         , D.FieldDescriptorProto.type_name = type_name })
    = ([ident name],HsUnBangedTy (labeled (HsTyCon typed)))
  where labeled = case labelEnum of
                    LABEL_OPTIONAL -> typeApp "Maybe"
                    LABEL_REPEATED -> typeApp "Seq"
                    LABEL_REQUIRED -> id
        typed,typedByName :: HsQName
        typed         = maybe typedByName typePrimitive type'
        typedByName   = maybe (error "No Name for Type!") qual type_name
        typePrimitive :: Type -> HsQName
        typePrimitive = maybe typedByName private . useType

-- HsDataDecl     SrcLoc DataOrNew HsContext HsName [HsName] [HsQualConDecl] [HsQName]
-- data HsQualConDecl = HsQualConDecl SrcLoc {-forall-} [HsTyVarBind] {- . -} HsContext {- => -} HsConDecl
-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
descriptorX :: D.DescriptorProto -> HsDecl
descriptorX (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = HsDataDecl src DataType [] (ident name) [] [con] derives
        where con = HsQualConDecl src [] [] (HsRecDecl (ident name) fields)
                  where fields = F.foldr ((:) . fieldX) [] field

-- | HsInstDecl     SrcLoc HsContext HsQName [HsType] [HsInstDecl]
instancesDescriptor :: D.DescriptorProto -> [HsDecl]
instancesDescriptor d = instanceMergeable d ++ [instanceDefault d]

instanceDefault :: D.DescriptorProto -> HsDecl
instanceDefault (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = HsInstDecl src [] (private "Default") [HsTyCon (qual name)] []

instanceMergeable :: D.DescriptorProto -> [HsDecl]
instanceMergeable (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = [ HsInstDecl src [] (private "Mergeable") [HsTyCon (qual name)]
        [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeEmpty") [] 
                                (HsUnGuardedRhs (Prelude.foldl1 HsApp (HsCon (qual name) : replicate len (HsCon (private "mergeEmpty")))))
                                noWhere])
        , HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeAppend") [ HsPApp (qual name) patternVars1
                                                                    , HsPApp (qual name) patternVars2]
                                (HsUnGuardedRhs (Prelude.foldl1 HsApp (HsCon (qual name) : 
                                                                       (zipWith append vars1 vars2)
                                              ))) noWhere])
        ]
      , HsInstDecl src [] (private "Mergeable") [typeApp "Maybe" $ HsTyCon (qual name)]
        [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeEmpty") [] 
                                (HsUnGuardedRhs (HsCon (private "Nothing"))) noWhere])
        , HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeAppend") [] 
                                (HsUnGuardedRhs (HsVar (private "mayMerge"))) noWhere])
        ]
      ]
  where len = S.length field
        con = HsCon (qual name)
        patternVars1 :: [HsPat]
        patternVars1 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("x'" ++ show n))) [1..]
        patternVars2 :: [HsPat]
        patternVars2 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("y'" ++ show n))) [1..]
        vars1 :: [HsExp]
        vars1 = take len inf
            where inf = map (\n -> HsVar (UnQual (HsIdent ("x'" ++ show n)))) [1..]
        vars2 :: [HsExp]
        vars2 = take len inf
            where inf = map (\n -> HsVar (UnQual (HsIdent ("y'" ++ show n)))) [1..]
        append x y = HsParen $ HsApp (HsApp (HsVar (private "mergeAppend")) x) y

------------------------------------------------------------------

derives :: [HsQName]
derives = map private ["Show","Read","Eq","Ord","Data","Typeable"]

useType :: Type -> Maybe String
useType TYPE_DOUBLE   = Just "Double"
useType TYPE_FLOAT    = Just "Float"
useType TYPE_BOOL     = Just "Bool"
useType TYPE_STRING   = Just "ByteString"
useType TYPE_BYTES    = Just "ByteString"
useType TYPE_UINT32   = Just "Word32"
useType TYPE_FIXED32  = Just "Word32"
useType TYPE_UINT64   = Just "Word64"
useType TYPE_FIXED64  = Just "Word64"
useType TYPE_INT32    = Just myInt32
useType TYPE_SINT32   = Just myInt32
useType TYPE_SFIXED32 = Just myInt32
useType TYPE_INT64    = Just "Int64"
useType TYPE_SINT64   = Just "Int64"
useType TYPE_SFIXED64 = Just "Int64"
useType TYPE_MESSAGE  = Nothing
useType TYPE_ENUM     = Nothing
useType TYPE_GROUP    = error "<TYPE_GROUP IS UNIMPLEMENTED>"

noWhere = (HsBDecls [])

testD = putStrLn . prettyPrint . descriptorModule "Text" $ d

test = putStrLn  . unlines . Prelude.concat $
         [ map prettyPrint ( descriptorX d : instancesDescriptor d )
         ]
d :: D.DescriptorProto.DescriptorProto
d = defaultValue
    { D.DescriptorProto.name = Just (BSC.pack "SomeMod.ServiceOptions") 
    , D.DescriptorProto.field = S.fromList
       [ defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field1TestPrimitive")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field2TestSelf")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (BSC.pack "ServiceOptions")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field3TestQualified")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (BSC.pack "A.B.C.Label")
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field4TestUnqualified")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (BSC.pack "Maybe")
         }
       ]
    }
{-
-- http://haskell.org/onlinereport/lexemes.html#sect2.4
-- The .proto parser should have handled this
mangle :: String -> String
mangle s = fromMaybe s (M.lookup s m)
  where m = M.fromAscList . map (\t -> (t,t++"'") $ reserved
        reserved = ["case","class","data","default","deriving","do","else"
                   ,"if","import","in","infix","infixl","infixr","instance"
                   ,"let","module","newtype","of","then","type","where"]
-}