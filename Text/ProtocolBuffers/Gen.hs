-- Turn *Proto into Language.Haskell.Exts.Syntax from haskell-src-exts package
module Text.ProtocolBuffers.Gen where
import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
{-
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto(ExtensionRange)
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.DescriptorProto.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..)) 
import qualified Text.DescriptorProtos.EnumOptions                    as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions                    as D.EnumOptions(EnumOptions(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueOptions               as D(EnumValueOptions) 
import qualified Text.DescriptorProtos.EnumValueOptions               as D.EnumValueOptions(EnumValueOptions(..)) 
-}
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import           Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
{-
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions(CType)
import qualified Text.DescriptorProtos.FieldOptions.CType             as D.FieldOptions.CType(CType(..))
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto) 
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions(OptimizeMode)
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode       as D.FileOptions.OptimizeMode(OptimizeMode(..))
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as U

import Text.ProtocolBuffers.Header

import Data.Sequence(Seq)
import qualified Data.Sequence as S
import Data.Foldable as F

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty

-- This is me playing around to learn the whole module syntax tree
--src = (SrcLoc {srcFilename = "/tmp/M.hs",srcLine=1,srcColumn=1})
m = HsModule src
             (Module "A.B.C")
             (Just [HsEVar (name "x")])
             ([HsImportDecl src (Module "Data.Int") True (Just (Module "In"))
                            (Just (True, [HsIAbs (HsIdent "Int8")
                                         ,HsIAbs (HsIdent "Int16")
                                         ]))
              ])
             [HsTypeSig src [HsIdent "x"] (HsTyCon (Qual (Module "In") (HsIdent "Int32")))
             ,HsPatBind src (HsPVar (HsIdent "x")) (HsUnGuardedRhs (HsLit (HsInt 11)))
                        (HsBDecls [HsPatBind src (HsPVar (HsIdent "y")) (HsUnGuardedRhs (HsLit (HsInt 12))) 
                                             (HsBDecls [])
                                  ])
             ,HsSpliceDecl src (HsParenSplice (HsApp (HsVar (name "deriveIt")) (HsReifyExp (HsReifyDecl (name "Integer")))))
             ]
  where name s = UnQual (HsIdent s)

print_m = putStrLn (prettyPrint m)  
{-
module A.B.C (x) where
import qualified Data.Int as In hiding (Int8, Int16)
 
x :: In.Int32
x = 11
  where y = 12
-}

{- ***************************************************************************************** -}
-- 'descriptorX' renders a DescriptorProto to a data declaration.
-- 'fieldX' renders a FieldDescriptorProto to an entry in a data declaration.
{- ***************************************************************************************** -}

src :: SrcLoc
src = error "No SrcLoc"

ident :: ByteString -> HsName
ident bs = HsIdent (U.toString bs)

typeApp :: String -> HsType -> HsType
typeApp s =  HsTyApp (HsTyCon (UnQual (HsIdent s)))

-- 'qual' and 'qmodname' are only correct for simple or fully looked-up names.
qual :: ByteString -> HsQName
qual bs = case BSC.spanEnd ('.'/=) bs of
            (pre,post) | BSC.length pre <= 1 -> UnQual (ident bs)
                       | otherwise -> qmodname (BSC.init pre) post

qmodname :: ByteString -> ByteString -> HsQName
qmodname modName typeName = Qual (Module (U.toString modName)) (ident typeName)

-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
fieldX :: D.FieldDescriptorProto -> ([HsName],HsBangType)
fieldX (D.FieldDescriptorProto.FieldDescriptorProto
         { D.FieldDescriptorProto.name = Just name
         , D.FieldDescriptorProto.label = Just labelEnum
         , D.FieldDescriptorProto.type' = type'
         , D.FieldDescriptorProto.type_name = type_name
         })
    = ([ident name],HsUnBangedTy (labeled (HsTyCon typed)))
  where labeled = case labelEnum of
                    LABEL_OPTIONAL -> typeApp "Maybe"
                    LABEL_REPEATED -> typeApp "Seq"
                    LABEL_REQUIRED -> id
        typed,typedByName :: HsQName
        typed         = maybe typedByName typePrimitive type'
        typedByName   = maybe (error "No Name for Type!") qual type_name
        typePrimitive :: Type -> HsQName
        typePrimitive = maybe typedByName (UnQual . HsIdent) . useType

-- HsDataDecl     SrcLoc DataOrNew HsContext HsName [HsName] [HsQualConDecl] [HsQName]
-- data HsQualConDecl = HsQualConDecl SrcLoc {-forall-} [HsTyVarBind] {- . -} HsContext {- => -} HsConDecl
-- data HsConDecl = HsConDecl HsName [HsBangType] -- ^ ordinary data constructor
--                | HsRecDecl HsName [([HsName],HsBangType)] -- ^ record constructor
descriptorX :: D.DescriptorProto -> HsDecl
descriptorX (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field
              })
    = HsDataDecl src DataType [] (ident name) [] [con] derives
        where con = HsQualConDecl src [] [] (HsRecDecl (ident name) fields)
                  where fields = F.foldr ((:) . fieldX) [] field

derives :: [HsQName]
derives = map (UnQual . HsIdent) ["Show","Eq","Ord","Data","Typeable"]

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
useType TYPE_INT32    = Just "Int32"
useType TYPE_SINT32   = Just "Int32"
useType TYPE_SFIXED32 = Just "Int32"
useType TYPE_INT64    = Just "Int64"
useType TYPE_SINT64   = Just "Int64"
useType TYPE_SFIXED64 = Just "Int64"
useType TYPE_MESSAGE  = Nothing
useType TYPE_ENUM     = Nothing
useType TYPE_GROUP    = error "<TYPE_GROUP IS UNIMPLEMENTED>"
