-- Turn *Proto into Language.Haskell.Exts.Syntax from haskell-src-exts package
-- Need to get this just far enough to allow bootstrapping of 'descriptor.proto'
-- Done: Enum modules (given a prefix)
-- TODO: Message Descriptors (the imports are a mess)
--   These can only "see" names of siblings
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

import Text.ProtocolBuffers.Header

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import Data.List(sort)
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
             [HsImportDecl src (Module "Data.Int") True (Just (Module "In"))
                               (Just (True, [HsIAbs (HsIdent "Int8")
                                            ,HsIAbs (HsIdent "Int16")
                                            ]))
              ]
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
typeApp s =  HsTyApp (HsTyCon (private s))

-- 'qual' and 'qmodname' are only correct for simple or fully looked-up names.
qual :: ByteString -> HsQName
qual bs = case BSC.spanEnd ('.'/=) bs of
            (pre,post) | BSC.length pre <= 1 -> UnQual (ident bs)
                       | otherwise -> qmodname (BSC.init pre) post

qmodname :: ByteString -> ByteString -> HsQName
qmodname modName typeName = Qual (Module (U.toString modName)) (ident typeName)

private :: String -> HsQName
private t = Qual (Module "P'") (HsIdent t)


--------------------------------------------
-- EnumDescriptorProto module creation
--------------------------------------------

testLabel = putStrLn . prettyPrint $ enumModule "Text.DescriptorProtos.FieldDescriptorProto" e

e :: D.EnumDescriptorProto.EnumDescriptorProto
e = defaultValue
    { D.EnumDescriptorProto.name = Just (BSC.pack "Label")
    , D.EnumDescriptorProto.value = S.fromList
      [ defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_OPTIONAL")
                     , D.EnumValueDescriptorProto.number = Just 1 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_REQUIRED")
                     , D.EnumValueDescriptorProto.number = Just 2 }
      , defaultValue { D.EnumValueDescriptorProto.name = Just (BSC.pack "LABEL_REPEATED")
                     , D.EnumValueDescriptorProto.number = Just 3 }
      ]
    }

enumModule :: String -> D.EnumDescriptorProto -> HsModule
enumModule prefix
           e@(D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just rawName}) =
  let name = U.toString rawName
      fqModuleName = prefix ++ "." ++ name
  in HsModule src
         (Module fqModuleName)
         (Just [HsEThingAll (UnQual (HsIdent name))])
         [ HsImportDecl src (Module "Prelude") True (Just (Module "P'")) Nothing
         , HsImportDecl src (Module "Text.ProtocolBuffers.Header") True (Just (Module "P'")) Nothing
         ]
         (enumDecls e)
  
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
         { D.EnumDescriptorProto.name = Just name})
    = HsDataDecl src DataType [] (ident name) [] (map enumValueX values) derives
        where values = enumValues e

instanceBounded :: D.EnumDescriptorProto -> HsDecl
instanceBounded e@(D.EnumDescriptorProto.EnumDescriptorProto
                   { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Bounded") [HsTyCon (qual name)] 
                 (map (HsInsDecl . HsFunBind) [set "minBound" (head values)
                                              ,set "maxBound" (last values)]) where
        values = enumValues e
        set f (_,n) = [HsMatch src (HsIdent f) [] (HsUnGuardedRhs (HsCon (UnQual n))) noWhere]

instanceEnum :: D.EnumDescriptorProto -> HsDecl
instanceEnum e@(D.EnumDescriptorProto.EnumDescriptorProto
                { D.EnumDescriptorProto.name = Just name})
    = HsInstDecl src [] (private "Enum") [HsTyCon (qual name)] 
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
        
instancesEnum :: D.EnumDescriptorProto -> [HsDecl]
instancesEnum e = instanceMergeableEnum e ++ [ instanceEnum e ,instanceBounded e ]

enumDecls :: D.EnumDescriptorProto -> [HsDecl]
enumDecls e = Prelude.concat [ [enumX e]
                             , instancesEnum e
                             , litEnumValues e
                             ]

instanceMergeableEnum :: D.EnumDescriptorProto -> [HsDecl]
instanceMergeableEnum (D.EnumDescriptorProto.EnumDescriptorProto
              { D.EnumDescriptorProto.name = Just name }) =
    [ HsInstDecl src [] (private "Mergeable") [HsTyCon (qual name)] []
    , HsInstDecl src [] (private "Mergeable") [typeApp "Maybe" $ HsTyCon (qual name)]
      [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeEmpty") [] 
                                          (HsUnGuardedRhs (HsCon (private "Nothing"))) noWhere])
      , HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeAppend") [] 
                                          (HsUnGuardedRhs (HsVar (private "mayMerge"))) noWhere])
      ]
    ]

litEnumValues :: D.EnumDescriptorProto -> [HsDecl]
litEnumValues e@(D.EnumDescriptorProto.EnumDescriptorProto
                 { D.EnumDescriptorProto.name = Just name }) = 
    [ HsTypeSig src [HsIdent "ascList'"]
                    (HsTyApp (HsTyCon (UnQual (HsIdent "[]")))
                             (HsTyTuple Boxed [ HsTyCon (private "Integer")
                                              , HsTyCon (private "String")
                                              , HsTyCon (qual name)
                                              ]))
    , HsPatBind src (HsPVar (HsIdent "ascList'"))
                    (HsUnGuardedRhs (HsList (map one values))) noWhere
    ]
  where one (v,n@(HsIdent ns)) = HsTuple [HsLit (HsInt v),HsLit (HsString ns),HsCon (UnQual n)]
        values = enumValues e

-- instanceDefaultEnum :: D.EnumDescriptorProto -> HsDecl
-- instanceDefaultEnum (D.EnumDescriptorProto.EnumDescriptorProto
--               { D.EnumDescriptorProto.name = Just name
--               })
--     = HsInstDecl src [] (private "Default") [HsTyCon (qual name)] []

--------------------------------------------
-- DescriptorProto module creation is unfinished
--   There are difficult namespace issues
--------------------------------------------

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
        typePrimitive = maybe typedByName private . useType

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

-- | HsInstDecl     SrcLoc HsContext HsQName [HsType] [HsInstDecl]
instancesDescriptor :: D.DescriptorProto -> [HsDecl]
instancesDescriptor d = instanceMergeable d ++ [instanceDefault d]

instanceDefault :: D.DescriptorProto -> HsDecl
instanceDefault (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field
              })
    = HsInstDecl src [] (private "Default") [HsTyCon (qual name)] []

instanceMergeable :: D.DescriptorProto -> [HsDecl]
instanceMergeable (D.DescriptorProto.DescriptorProto
              { D.DescriptorProto.name = Just name
              , D.DescriptorProto.field = field
              }) =
    [ HsInstDecl src [] (private "Mergeable") [HsTyCon (qual name)]
      [ HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeEmpty") [] 
        (HsUnGuardedRhs (Prelude.foldl1 HsApp (HsCon (qual name) : replicate len (HsCon (private "mergeEmpty")))))
                                          noWhere])

      , HsInsDecl (HsFunBind [HsMatch src (HsIdent "mergeAppend") [ HsPApp (qual name) patternVars1
                                                                  , HsPApp (qual name) patternVars2]
        (HsUnGuardedRhs (Prelude.foldl1 HsApp (HsCon (qual name) : 
                                               (zipWith (\x y -> HsParen $ HsApp (HsApp (HsVar (private "mergeAppend")) x) y) vars1 vars2)
                                              ))) noWhere])



--                                          (HsUnGuardedRhs (HsVar (private "mergeAppend"))) noWhere])
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


derives :: [HsQName]
derives = map private ["Show","Eq","Ord","Data","Typeable"]

int32 :: String
int32 = "Int32"

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
useType TYPE_INT32    = Just int32
useType TYPE_SINT32   = Just int32
useType TYPE_SFIXED32 = Just int32
useType TYPE_INT64    = Just "Int64"
useType TYPE_SINT64   = Just "Int64"
useType TYPE_SFIXED64 = Just "Int64"
useType TYPE_MESSAGE  = Nothing
useType TYPE_ENUM     = Nothing
useType TYPE_GROUP    = error "<TYPE_GROUP IS UNIMPLEMENTED>"

noWhere = (HsBDecls [])

test = putStrLn  . unlines . Prelude.concat $
         [ map prettyPrint ( descriptorX d : instancesDescriptor d )
         ]
d :: D.DescriptorProto.DescriptorProto
d = defaultValue
    { D.DescriptorProto.name = Just (BSC.pack "ServiceOptions") 
    , D.DescriptorProto.field = S.fromList
       [ defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field1")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_STRING
         }
       , defaultValue
         { D.FieldDescriptorProto.name = Just (BSC.pack "field2")
         , D.FieldDescriptorProto.label = Just LABEL_OPTIONAL
         , D.FieldDescriptorProto.type' = Just TYPE_MESSAGE
         , D.FieldDescriptorProto.type_name = Just (BSC.pack "ServiceOptions")
         }
       ]
    }


-- http://haskell.org/onlinereport/lexemes.html#sect2.4
-- The .proto parser should have handled this
mangle :: String -> String
mangle s = fromMaybe s (M.lookup s m)
  where m = M.fromAscList (map (\t -> (t,t++"'"))
              ["case","class","data","default","deriving","do","else"
              ,"if","import","in","infix","infixl","infixr","instance"
              ,"let","module","newtype","of","then","type","where"])
