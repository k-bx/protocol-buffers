-- try "test", "testDesc", and "testLabel" to see sample output
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
-- The above fails to mentions enums' default is their first value (always exists)
-- see something like http://msdn.microsoft.com/en-us/library/6aw8xdf2.aspx for c escape parsing
-- So rendering a message to text is ugly, the strings are valid UTF-8 and so are rendered in
-- some other unicode form, while bytes must be unparsed to use. The only sane thing to do is
-- put a parsed and converted Haskell data type into the defaults interface.
-- This has been done in Reflections.hs
module Text.ProtocolBuffers.Gen(descriptorModule,enumModule,prettyPrint) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
{- not yet used
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
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
{- -- unused or unusable
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions(CType)
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions.CType(CType(..))
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions(OptimizeMode)
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions.OptimizeMode(OptimizeMode(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions(MessageOptions(..))
-}
{-  -- related to the rpc system
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.MethodOptions                  as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions                  as D.MethodOptions(MethodOptions(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto) 
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..)) 
import qualified Text.DescriptorProtos.ServiceOptions                 as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions                 as D.ServiceOptions(ServiceOptions(..))
-}

--import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Default
import Text.ProtocolBuffers.Reflections
import Text.ProtocolBuffers.WireMessage(size'Varint)

import qualified Data.ByteString(concat)
import qualified Data.ByteString.Char8(spanEnd)
import qualified Data.ByteString.Lazy.Char8 as BSC(toChunks,fromChunks,length,init,unpack)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString,toString)
import Data.Bits(Bits((.|.),shiftL))
import Data.Maybe(fromMaybe,catMaybes)
import Data.List(sort,group,foldl',foldl1')
import Data.Sequence(viewl,ViewL(..))
import qualified Data.Sequence as Seq(length,fromList)
import Data.Foldable as F(foldr,toList)
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

-- -- -- -- Helper functions

($$) :: HsExp -> HsExp -> HsExp
($$) = HsApp

infixl 1 $$

utf8FromString = Utf8 . U.fromString

toWireTag :: FieldId -> FieldType -> WireTag
toWireTag fieldId fieldType
    = ((fromIntegral . getFieldId $ fieldId) `shiftL` 3) .|. (fromIntegral . getWireType . toWireType $ fieldType)
{-
  enum WireType {
    WIRETYPE_VARINT           = 0,
    WIRETYPE_FIXED64          = 1,
    WIRETYPE_LENGTH_DELIMITED = 2,
    WIRETYPE_START_GROUP      = 3,
    WIRETYPE_END_GROUP        = 4,
    WIRETYPE_FIXED32          = 5, };

    TYPE_DOUBLE         = 1;
    TYPE_FLOAT          = 2;
    TYPE_INT64          = 3;
    TYPE_UINT64         = 4;
    TYPE_INT32          = 5;
    TYPE_FIXED64        = 6;
    TYPE_FIXED32        = 7;
    TYPE_BOOL           = 8;
    TYPE_STRING         = 9;
    TYPE_GROUP          = 10;  // Tag-delimited aggregate.
    TYPE_MESSAGE        = 11;
    TYPE_BYTES          = 12;
    TYPE_UINT32         = 13;
    TYPE_ENUM           = 14;
    TYPE_SFIXED32       = 15;
    TYPE_SFIXED64       = 16;
    TYPE_SINT32         = 17;
    TYPE_SINT64         = 18; -}
-- http://code.google.com/apis/protocolbuffers/docs/encoding.html
toWireType :: FieldType -> WireType
toWireType  1 =  1
toWireType  2 =  5
toWireType  3 =  0
toWireType  4 =  0
toWireType  5 =  0
toWireType  6 =  1
toWireType  7 =  5
toWireType  8 =  0
toWireType  9 =  2
toWireType 10 =  3 -- START_GROUP
toWireType 11 =  2
toWireType 12 =  2
toWireType 13 =  0
toWireType 14 =  0
toWireType 15 =  5
toWireType 16 =  1
toWireType 17 =  5
toWireType 18 =  1
toWireType  x = error $ "Text.ProcolBuffers.Gen.toWireType: Bad FieldType: "++show x

dotPre :: String -> String -> String
dotPre "" = id
dotPre s | '.' == last s = (s ++)
         | otherwise = (s ++) . ('.':)

spanEndL f bs = let (a,b) = Data.ByteString.Char8.spanEnd f (Data.ByteString.concat . BSC.toChunks $ bs)
                in (BSC.fromChunks [a],BSC.fromChunks [b])

-- Take a bytestring of "A" into "Right A" and "A.B.C" into "Left (A.B,C)"
splitMod :: Utf8 -> Either (Utf8,Utf8) Utf8
splitMod (Utf8 bs) = case spanEndL ('.'/=) bs of
                       (pre,post) | BSC.length pre <= 1 -> Right (Utf8 bs)
                                  | otherwise -> Left (Utf8 (BSC.init pre),Utf8 post)

unqual :: Utf8 -> HsQName
unqual bs = UnQual (base bs)

base :: Utf8 -> HsName
base bs = case splitMod bs of
            Right typeName -> (ident typeName)
            Left (_,typeName) -> (ident typeName)

src :: SrcLoc
src = SrcLoc "No SrcLoc" 0 0

toString = U.toString . utf8

litInt :: Integral x => x -> HsExp
litInt x = HsLit (HsInt (toInteger x))

ident :: Utf8 -> HsName
ident bs = HsIdent (toString bs)

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (private s))

-- 'qual' and 'qmodname' are only correct for simple or fully looked-up names.
qual :: Utf8 -> HsQName
qual bs = case splitMod bs of
            Right typeName -> UnQual (ident typeName)
            Left (modName,typeName) -> Qual (Module (toString modName)) (ident typeName)

pvar :: String -> HsExp
pvar t = HsVar (private t)

lvar :: String -> HsExp
lvar t = HsVar (UnQual (HsIdent t))

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)

inst s p r  = HsInsDecl (HsFunBind [HsMatch src (HsIdent s) p (HsUnGuardedRhs r) noWhere])

--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------


fqName :: ProtoName -> String
fqName (ProtoName a b c) = dotPre a (dotPre b c)

enumModule :: String -> D.EnumDescriptorProto -> HsModule
enumModule prefix
           e@(D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just rawName})
    = let protoName = case splitMod rawName of
                        Left (m,b) -> ProtoName prefix (toString m) (toString b)
                        Right b    -> ProtoName prefix ""             (toString b)
      in HsModule src (Module (fqName protoName))
                  (Just [HsEThingAll (UnQual (HsIdent (baseName protoName)))])
                  standardImports (enumDecls protoName e)

enumValues :: D.EnumDescriptorProto -> [(Integer,HsName)]
enumValues (D.EnumDescriptorProto.EnumDescriptorProto
            { D.EnumDescriptorProto.value = value}) 
    = sort $ F.foldr ((:) . oneValue) [] value
  where oneValue  :: D.EnumValueDescriptorProto -> (Integer,HsName)
        oneValue (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                  { D.EnumValueDescriptorProto.name = Just name
                  , D.EnumValueDescriptorProto.number = Just number })
            = (toInteger number,ident name)
      
enumX :: D.EnumDescriptorProto -> HsDecl
enumX e@(D.EnumDescriptorProto.EnumDescriptorProto
         { D.EnumDescriptorProto.name = Just rawName})
    = HsDataDecl src DataType [] (base rawName) [] (map enumValueX values) derives
        where values = enumValues e
              enumValueX :: (Integer,HsName) -> HsQualConDecl
              enumValueX (_,hsName) = HsQualConDecl src [] [] (HsConDecl hsName [])

enumDecls :: ProtoName -> D.EnumDescriptorProto -> [HsDecl]
enumDecls p e = enumX e :  [ instanceMergeableEnum e
                           , instanceBounded e
                           , instanceDefaultEnum e
                           , instanceEnum e
                           , instanceWireEnum e
                           , instanceReflectEnum p e ]

instanceBounded :: D.EnumDescriptorProto -> HsDecl
instanceBounded e@(D.EnumDescriptorProto.EnumDescriptorProto
                   { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Bounded") [HsTyCon (unqual name)] 
                 [set "minBound" (head values),set "maxBound" (last values)]
        where values = enumValues e
              set f (_,n) = inst f [] (HsCon (UnQual n))

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

-- fromEnum TYPE_ENUM == 14 :: Int
instanceWireEnum :: D.EnumDescriptorProto -> HsDecl
instanceWireEnum (D.EnumDescriptorProto.EnumDescriptorProto
                  { D.EnumDescriptorProto.name = Just name })
    = HsInstDecl src [] (private "Wire") [HsTyCon (unqual name)]
      [ withName "wireSize", withName "wirePut", withGet ]
  where withName foo = inst foo [HsPLit (HsInt 14),HsPVar (HsIdent "enum")] rhs
          where rhs = (pvar foo $$ HsLit (HsInt 14)) $$
                      (HsParen $ pvar "fromEnum" $$ lvar "enum")
        withGet = inst "wireGet" [HsPLit (HsInt 14)] rhs
          where rhs = (pvar "fmap" $$ pvar "toEnum") $$
                      (HsParen $ pvar "wireGet" $$ HsLit (HsInt 14))

instanceMergeableEnum :: D.EnumDescriptorProto -> HsDecl
instanceMergeableEnum (D.EnumDescriptorProto.EnumDescriptorProto
                       { D.EnumDescriptorProto.name = Just name }) =
    HsInstDecl src [] (private "Mergeable") [HsTyCon (unqual name)] []

{- from google's descriptor.h, about line 346:

  // Get the field default value if cpp_type() == CPPTYPE_ENUM.  If no
  // explicit default was defined, the default is the first value defined
  // in the enum type (all enum types are required to have at least one value).
  // This never returns NULL.

-}

instanceDefaultEnum :: D.EnumDescriptorProto -> HsDecl
instanceDefaultEnum edp@(D.EnumDescriptorProto.EnumDescriptorProto
                         { D.EnumDescriptorProto.name = Just name
                         , D.EnumDescriptorProto.value = value})
    = HsInstDecl src [] (private "Default") [HsTyCon (unqual name)]
      [ inst "defaultValue" [] firstValue ]
  where firstValue :: HsExp
        firstValue = case viewl value of
                       (:<) (D.EnumValueDescriptorProto.EnumValueDescriptorProto
                             { D.EnumValueDescriptorProto.name = Just name }) _ ->
                                 HsCon (UnQual (ident name))
                       EmptyL -> error $ "EnumDescriptorProto had empty sequence of EnumValueDescriptorProto.\n" ++ show edp

instanceReflectEnum :: ProtoName -> D.EnumDescriptorProto -> HsDecl
instanceReflectEnum protoName@(ProtoName a b c)
                    e@(D.EnumDescriptorProto.EnumDescriptorProto
                       { D.EnumDescriptorProto.name = Just rawName })
    = HsInstDecl src [] (private "ReflectEnum") [HsTyCon (unqual rawName)]
      [ inst "reflectEnum" [] ascList
      , inst "reflectEnumInfo" [ HsPWildCard ] ei
      ]
  where values = enumValues e
        ascList,ei,protoNameExp :: HsExp
        ascList = HsList (map one values)
          where one (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns),HsCon (UnQual n)]
        ei = foldl' HsApp (HsCon (private "EnumInfo")) [protoNameExp,HsList (map two values)]
          where two (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns)]
        protoNameExp = HsParen $ foldl' HsApp (HsCon (private "ProtoName")) [HsLit (HsString a)
                                                                            ,HsLit (HsString b)
                                                                            ,HsLit (HsString c)]

--------------------------------------------
-- DescriptorProto module creation is unfinished
--   There are difficult namespace issues
--------------------------------------------

descriptorModule :: Bool -> String -> D.DescriptorProto -> HsModule
descriptorModule isGroup prefix
                 d@(D.DescriptorProto.DescriptorProto
                    { D.DescriptorProto.name = Just rawName
                    , D.DescriptorProto.field = field })
    = let self = UnQual . HsIdent . toString . either snd id . splitMod $ rawName
          fqModuleName = Module (dotPre prefix (toString rawName))
          imports = standardImports ++ map formatImport (toImport d)
          protoName = case splitMod rawName of
                        Left (m,b) -> ProtoName prefix (toString m) (toString b)
                        Right b    -> ProtoName prefix ""             (toString b)
          (insts,di) = instancesDescriptor isGroup protoName d
      in HsModule src fqModuleName (Just [HsEThingAll self]) imports (descriptorX d : insts)
  where formatImport (Left (m,t)) = HsImportDecl src (Module (dotPre prefix (dotPre m t))) True
                                      (Just (Module m)) (Just (False,[HsIAbs (HsIdent t)]))
        formatImport (Right t)    = HsImportDecl src (Module (dotPre prefix t)) False
                                      Nothing (Just (False,[HsIAbs (HsIdent t)]))

standardImports = [ HsImportDecl src (Module "Prelude") False Nothing
                      (Just (False,[HsIVar (HsSymbol "+"),HsIVar (HsSymbol "++")]))
                  , HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
                  , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing ]

-- Create a list of (Module,Name) to import
-- Assumes that all self references are _not_ qualified!
toImport :: D.DescriptorProto -> [Either (String,String) String]
toImport msg@(D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = map head . group . sort
      . map (either (\(m,t) -> Left (toString m,toString t)) (Right . toString))
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
                answerName = maybe (error $ "No Name for Descriptor!\n" ++ show msg) Just type_name

-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
fieldX :: D.FieldDescriptorProto -> ([HsName],HsBangType)
fieldX fld@(D.FieldDescriptorProto.FieldDescriptorProto
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
        typedByName   = maybe (error $  "No Name for Field!\n" ++ show fld) qual type_name
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
    = HsDataDecl src DataType [] (base name) [] [con] derives
        where con = HsQualConDecl src [] [] (HsRecDecl (base name) fields)
                  where fields = F.foldr ((:) . fieldX) [] field

-- There is some confusing code below.  The FieldInfo and
-- DescriptorInfo are getting built as a "side effect" of
-- instanceDefault generating the instances for the Default class.
-- This DescriptorInfo information is then passed to
-- instanceReflectDescriptor to generate the instance of the
-- ReflectDescriptor class.

-- | HsInstDecl     SrcLoc HsContext HsQName [HsType] [HsInstDecl]
instancesDescriptor :: Bool -> ProtoName -> D.DescriptorProto -> ([HsDecl],DescriptorInfo)
instancesDescriptor isGroup protoName d = ([ instanceMergeable d, def, instanceWireDescriptor isGroup di, instanceReflectDescriptor di ],di)
  where (def,di) = instanceDefault protoName d

instanceReflectDescriptor :: DescriptorInfo -> HsDecl
instanceReflectDescriptor di
    = HsInstDecl src [] (private "ReflectDescriptor") [HsTyCon (UnQual (HsIdent (baseName (descName di))))]
        [ inst "reflectDescriptorInfo" [ HsPWildCard ] rdi ]
  where -- massive shortcut through show and read
        rdi :: HsExp
        rdi = pvar "read" $$ HsLit (HsString (show di))

instanceDefault :: ProtoName -> D.DescriptorProto -> (HsDecl,DescriptorInfo)
instanceDefault protoName (D.DescriptorProto.DescriptorProto
                           { D.DescriptorProto.name = Just name
                           , D.DescriptorProto.field = field })
    = ( HsInstDecl src [] (private "Default") [HsTyCon (unqual name)]
          [ inst "defaultValue" [] (foldl' HsApp (HsCon (unqual name)) deflist) ]
      , descriptorInfo )
  where len = Seq.length field
        old = replicate len (pvar "defaultValue")
        fieldInfos :: [FieldInfo]
        (deflist,fieldInfos) = unzip (F.foldr ((:) . defX) [] field)
        descriptorInfo = DescriptorInfo protoName (Seq.fromList fieldInfos)

defaultValueExp :: D.FieldDescriptorProto -> (HsExp,FieldInfo)
defaultValueExp  d@(D.FieldDescriptorProto.FieldDescriptorProto
                     { D.FieldDescriptorProto.name = Just rawName
                     , D.FieldDescriptorProto.number = Just number
                     , D.FieldDescriptorProto.label = Just label
                     , D.FieldDescriptorProto.type' = Just type'
                     , D.FieldDescriptorProto.type_name = mayTypeName
                     , D.FieldDescriptorProto.default_value = mayRawDef })
    = ( maybe (pvar "defaultValue") (HsParen . toSyntax) mayDef
      , fieldInfo )
  where toSyntax :: HsDefault -> HsExp
        toSyntax x = case x of
                       HsDef'Bool b -> HsCon (private (show b))
                       HsDef'ByteString bs -> pvar "pack" $$ HsLit (HsString (BSC.unpack bs)) -- XXX change to UTF-8
                       HsDef'Rational r -> HsLit (HsFrac r)
                       HsDef'Integer i -> HsLit (HsInt i)
        mayDef = parseDefaultValue d
        fieldInfo = let fieldId = (FieldId (fromIntegral number))
                        fieldType = (FieldType (fromEnum type'))
                        wireTag = toWireTag fieldId fieldType
                        wireTagLength = size'Varint (getWireTag wireTag)
                    in FieldInfo (toString rawName) fieldId wireTag wireTagLength
                                 (label == LABEL_REQUIRED) (label == LABEL_REPEATED)
                                 fieldType
                                 (fmap toString mayTypeName) (fmap utf8 mayRawDef) mayDef

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
                   TYPE_BOOL    -> return parseDefBool
                   TYPE_BYTES   -> return parseDefBytes
                   TYPE_DOUBLE  -> return parseDefDouble
                   TYPE_FLOAT   -> return parseDefFloat
                   TYPE_STRING  -> return parseDefString
                   _            -> return parseDefInteger
         case todo (utf8 bs) of
           Nothing -> error ("Could not parse the default value for "++show d)
           Just value -> return value

defX :: D.FieldDescriptorProto -> (HsExp,FieldInfo)
defX d@(D.FieldDescriptorProto.FieldDescriptorProto
         { D.FieldDescriptorProto.name = Just name
         , D.FieldDescriptorProto.label = Just labelEnum
         , D.FieldDescriptorProto.type' = type'
         , D.FieldDescriptorProto.type_name = type_name })
    =  ( HsParen $ case labelEnum of LABEL_OPTIONAL -> HsCon (private "Just") $$ dv
                                     _ -> dv
       , fi )
  where (dv,fi) = defaultValueExp d

instanceMergeable :: D.DescriptorProto -> HsDecl
instanceMergeable (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field })
    = HsInstDecl src [] (private "Mergeable") [HsTyCon (unqual name)]
        [ inst "mergeEmpty" [] (foldl' HsApp (HsCon (unqual name)) (replicate len (HsCon (private "mergeEmpty"))))
        , inst "mergeAppend" [ HsPApp (unqual name) patternVars1
                             , HsPApp (unqual name) patternVars2]
                             (foldl' HsApp (HsCon (unqual name)) (zipWith append vars1 vars2))
        ]
  where len = Seq.length field
        con = HsCon (qual name)
        patternVars1 :: [HsPat]
        patternVars1 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("x'" ++ show n))) [1..]
        patternVars2 :: [HsPat]
        patternVars2 = take len inf
            where inf = map (\n -> HsPVar (HsIdent ("y'" ++ show n))) [1..]
        vars1 :: [HsExp]
        vars1 = take len inf
            where inf = map (\n -> lvar ("x'" ++ show n)) [1..]
        vars2 :: [HsExp]
        vars2 = take len inf
            where inf = map (\n -> lvar ("y'" ++ show n)) [1..]
        append x y = HsParen $ pvar "mergeAppend" $$ x $$ y

instanceWireDescriptor :: Bool -> DescriptorInfo -> HsDecl
instanceWireDescriptor isGroup (DescriptorInfo { descName = protoName
                                               , fields = fieldInfos })
  = let typeInt = toInteger . fromEnum $ if isGroup then TYPE_GROUP else TYPE_MESSAGE
        myPType = HsPLit (HsInt typeInt)
        myType= HsLit (HsInt typeInt)
        me = UnQual (HsIdent (baseName protoName))
        mine = HsPApp me . take (Seq.length fieldInfos) . map (\n -> HsPVar (HsIdent ("x'" ++ show n))) $ [1..]
        vars = take (Seq.length fieldInfos) . map (\n -> lvar ("x'" ++ show n)) $ [1..]
        add a b = HsInfixApp a (HsQVarOp (UnQual (HsSymbol "+"))) b
        sizes | null sizesList = HsLit (HsInt 0)
              | otherwise = HsParen (foldl1' add sizesList)
          where sizesList =  zipWith toSize vars . F.toList $ fieldInfos
        toSize var fi = let f = if isRequired fi then "wireSizeReq"
                                  else if canRepeat fi then "wireSizeRep"
                                      else "wireSizeOpt"
                        in foldl' HsApp (pvar f) [ litInt (wireTagLength fi)
                                                 , litInt (getFieldType (typeCode fi))
                                                 , var]
        putMsgSize = HsQualifier $ pvar "putSize" $$
                       (HsParen $ foldl' HsApp (pvar "wireSize") [ myType , lvar "self'" ])
        putStmts = if isGroup then putStmtsContent else putMsgSize:putStmtsContent
          where putStmtsContent | null putStmtsList = [HsQualifier $ pvar "return" $$ HsCon (Special HsUnitCon)]
                                | otherwise = putStmtsList
                putStmtsList = zipWith toPut vars . F.toList $ fieldInfos
        toPut var fi = let f = if isRequired fi then "wirePutReq"
                                 else if canRepeat fi then "wirePutRep"
                                     else "wirePutOpt"
                       in HsQualifier $
                          foldl' HsApp (pvar f) [ litInt (getWireTag (wireTag fi))
                                                , litInt (getFieldType (typeCode fi))
                                                , var]
        whereUpdateSelf = HsBDecls [HsFunBind [HsMatch src (HsIdent "update'Self")
                            [HsPVar (HsIdent "field'Number") ,HsPVar (HsIdent "old'Self")]
                            (HsUnGuardedRhs (HsCase (lvar "field'Number") updateAlts)) noWhere]]
        updateAlts = map toUpdate (F.toList fieldInfos) ++ [HsAlt src HsPWildCard (HsUnGuardedAlt $
                       pvar "unknownField" $$ (lvar "field'Number")) noWhere]
        toUpdate fi = HsAlt src (HsPLit . HsInt . toInteger . getFieldId . fieldNumber $ fi) (HsUnGuardedAlt $ 
                        pvar "fmap" $$ (HsParen $ HsLambda src [HsPVar (HsIdent "new'Field")] $
                                          HsRecUpdate (lvar "old'Self") [HsFieldUpdate (UnQual . HsIdent . fieldName $ fi)
                                                                                       (labelUpdate fi)])
                                    $$ (HsParen (pvar "wireGet" $$ (litInt . getFieldType . typeCode $ fi)))) noWhere
        labelUpdate fi | canRepeat fi = pvar "append" $$ HsParen ((lvar . fieldName $ fi) $$ lvar "old'Self")
                                                      $$ lvar "new'Field"
                       | isRequired fi = qMerge (lvar "new'Field")
                       | otherwise = qMerge (HsCon (private "Just") $$ lvar "new'Field")
            where merges = map fromEnum [ TYPE_MESSAGE, TYPE_GROUP ]
                  qMerge x | fromIntegral (getFieldType (typeCode fi)) `elem` merges =
                               pvar "mergeAppend" $$ HsParen ((lvar . fieldName $ fi) $$ lvar "old'Self") $$ (HsParen x)
                           | otherwise = x

    in HsInstDecl src [] (private "Wire") [HsTyCon me]
        [ inst "wireSize" [myPType,mine] sizes
        , inst "wirePut" [myPType,HsPAsPat (HsIdent "self'") (HsPParen mine)] (HsDo putStmts)
        , HsInsDecl (HsFunBind [HsMatch src (HsIdent "wireGet") [myPType] (HsUnGuardedRhs $
                                  (pvar (if isGroup then "getBaseMessage" else "getMessage") $$
                                        lvar "update'Self")) whereUpdateSelf])
        ]

------------------------------------------------------------------

derives :: [HsQName]
derives = map private ["Show","Read","Eq","Ord","Data","Typeable"]

useType :: Type -> Maybe String
useType TYPE_DOUBLE   = Just "Double"
useType TYPE_FLOAT    = Just "Float"
useType TYPE_BOOL     = Just "Bool"
useType TYPE_STRING   = Just "Utf8"
useType TYPE_BYTES    = Just "ByteString"
useType TYPE_UINT32   = Just "Word32"
useType TYPE_FIXED32  = Just "Word32"
useType TYPE_UINT64   = Just "Word64"
useType TYPE_FIXED64  = Just "Word64"
useType TYPE_INT32    = Just "Int32"
useType TYPE_SINT32   = Just "Int32"
useType TYPE_SFIXED32 = Just "Int32"
useType TYPE_INT64    = Just "Int64"
useType TYPE_SINT64   = Just "Int64"
useType TYPE_SFIXED64 = Just "Int64"
useType TYPE_MESSAGE  = Nothing
useType TYPE_ENUM     = Nothing
useType TYPE_GROUP    = Nothing

noWhere = (HsBDecls [])

test = putStrLn . prettyPrint . descriptorModule False "Text" $ d

testDesc =  putStrLn . prettyPrint . descriptorModule False "Text" $ genFieldOptions

testLabel = putStrLn . prettyPrint $ enumModule "Text" labelTest
testType = putStrLn . prettyPrint $ enumModule "Text" t

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
d :: D.DescriptorProto.DescriptorProto
d = defaultValue
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
