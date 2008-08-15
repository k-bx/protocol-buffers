-- | Text.ProtocolBuffers.Resolve takes the output of Text.ProtocolBuffers.Parse and runs all
-- the preprocessing and sanity checks that precede Text.ProtocolBuffers.Gen creating modules.
--
-- Currently this involves mangling the names, building a NameSpace (or [NameSpace]), and making
-- all the names fully qualified (and setting TYPE_MESSAGE or TYPE_ENUM) as appropriate.
-- Field names are also checked against a list of reserved words, appending a single quote
-- to disambiguate.
-- All names from Parser should start with a letter, but _ is also handled by replacing with U' or u'.
-- Anything else will trigger a "subborn ..." error.
-- Name resolution failure are not handled elegantly: it will kill the system with a long error message.
--
-- TODO: treat names with leading "." as already "fully-qualified"
--       make sure the optional fields that will be needed are not Nothing
--       check enum default values are allowed symbols
module Text.ProtocolBuffers.Resolve(resolveFDP) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto)
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto(EnumValueDescriptorProto))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto(FieldDescriptorProto))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions(MessageOptions(..))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto(MethodDescriptorProto))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..))

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Parser

import Control.Monad
import Data.Char
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Maybe
import Data.Map(Map)
import qualified Data.Map as M
import Data.List(unfoldr,span,inits,foldl')
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as LC

err s = error $ "Text.ProtocolBuffers.Resolve fatal error encountered, message:\n"++indent s
  where indent = unlines . map (\s -> ' ':' ':s) . lines

newlineBefore s = go where
  go [] = []
  go (x:xs) | x `elem` s = '\n':x:go xs
            | otherwise = x:go xs

encodeModuleNames :: [String] -> Utf8
encodeModuleNames [] = Utf8 mempty
encodeModuleNames xs = Utf8 . U.fromString . foldr1 (\a b -> a ++ '.':b) $ xs

mangleModuleNames :: Utf8 -> [String]
mangleModuleNames bs = map mangleModuleName . splitDot . U.toString . utf8 $ bs 

mangleCap :: Maybe Utf8 -> [String]
mangleCap = mangleModuleNames . fromMaybe (Utf8 mempty)

mangleCap1 :: Maybe Utf8 -> String
mangleCap1 = mangleModuleName . U.toString . utf8 . fromMaybe (Utf8 mempty)

splitDot :: String -> [String]
splitDot = unfoldr s where
  s ('.':xs) = s xs
  s [] = Nothing
  s xs = Just (span ('.'/=) xs)

mangleModuleName :: String -> String
mangleModuleName [] = "Empty'Name"
mangleModuleName ('_':xs) = "U'"++xs
mangleModuleName (x:xs) | isLower x = let x' = toUpper x
                                      in if isLower x' then error ("subborn lower case"++show (x:xs))
                                           else x': xs
mangleModuleName xs = xs

mangleFieldName :: Maybe Utf8 -> Maybe Utf8
mangleFieldName = fmap (Utf8 . U.fromString . fixname . U.toString . utf8)
  where fixname [] = "empty'name"
        fixname ('_':xs) = "u'"++xs
        fixname (x:xs) | isUpper x = let x' = toLower x
                                     in if isUpper x' then error ("stubborn upper case: "++show (x:xs))
                                          else fixname (x':xs)
        fixname xs | xs `elem` reserved = xs ++ "'"
        fixname xs = xs

reserved :: [String]
reserved = ["case","class","data","default","deriving","do","else"
           ,"if","import","in","infix","infixl","infixr","instance"
           ,"let","module","newtype","of","then","type","where"] -- also reserved is "_"

newtype NameSpace = NameSpace {unNameSpace::(Map String ([String],NameType,Maybe NameSpace))}
  deriving (Show,Read)
data NameType = Message | Enumeration | Void
  deriving (Show,Read)

type Context = [NameSpace]
type Resolver = Context -> ByteString -> ByteString

seeContext :: Context -> [String] 
seeContext cx = map ((++"[]") . concatMap (\k -> show k ++ ", ") . M.keys . unNameSpace) cx

data Box a = Box a
instance Show (Box a) where show (Box {}) = "Box"
test = do
  (Right fdp) <- pbParse filename2
  return (Box (fdp,resolveFDP fdp))

resolveFDP :: D.FileDescriptorProto -> D.FileDescriptorProto
resolveFDP protoIn =
  let prefix :: [String]
      prefix = mangleCap . msum $
                 [ D.FileOptions.java_outer_classname =<< (D.FileDescriptorProto.options protoIn)
                 , D.FileOptions.java_package =<< (D.FileDescriptorProto.options protoIn)
                 , D.FileDescriptorProto.package protoIn]
      -- Make top-most root NameSpace
      nameSpace = fromMaybe (NameSpace mempty) $ foldr addPrefix protoNames $ zip prefix (tail (inits prefix))
        where addPrefix (s1,ss) ns = Just . NameSpace $ M.singleton s1 (ss,Void,ns)
              protoNames | null protoMsgs = Nothing
                         | otherwise = Just . NameSpace . M.fromList $ protoMsgs
                where protoMsgs = F.foldr ((:) . msgNames prefix) protoEnums (D.FileDescriptorProto.message_type protoIn)
                      protoEnums = F.foldr ((:) . enumNames prefix) [] (D.FileDescriptorProto.enum_type protoIn)
                      msgNames context dIn =
                        let s1 = mangleCap1 (D.DescriptorProto.name dIn)
                            ss' = context ++ [s1]
                            dNames | null dMsgs = Nothing
                                   | otherwise = Just . NameSpace . M.fromList $ dMsgs
                            dMsgs = F.foldr ((:) . msgNames ss') dEnums (D.DescriptorProto.nested_type dIn)
                            dEnums = F.foldr ((:) . enumNames ss') [] (D.DescriptorProto.enum_type dIn)
                        in ( s1 , (ss',Message,dNames) )
                      enumNames context eIn =
                        let s1 = mangleCap1 (D.EnumDescriptorProto.name eIn)
                        in ( s1 , (context ++ [s1],Enumeration,Nothing) )
      -- Context stack for resolving the top level declarations
      protoContext :: Context
      protoContext = foldl' (\nss@(NameSpace ns:_) pre -> case M.lookup pre ns of
                                                            Just (_,Void,Just ns1) -> (ns1:nss)
                                                            _ -> nss) [nameSpace] prefix
      descend :: Context -> Maybe Utf8 -> Context
      descend cx@(NameSpace n:_) name =
        case M.lookup mangled n of
          Just (_,_,Nothing) -> cx
          Just (_,_,Just ns1) -> ns1:cx
          x -> error $ "Name resolution failed:\n"++unlines (mangled : show x : "KNOWN NAMES" : seeContext cx)
       where mangled = mangleCap1 name
      resolve :: Context -> Maybe Utf8 -> Maybe Utf8
      resolve context bsIn = fmap fst (resolve2 context bsIn)
      resolve2 :: Context -> Maybe Utf8 -> Maybe (Utf8,NameType)
      resolve2 context Nothing = Nothing
      resolve2 context bsIn = let nameIn = mangleCap bsIn
                                  resolver [] (NameSpace cx) = error $ "resolve2.resolver []\n"++unlines [show bsIn,show nameIn,show (M.keys cx)]
                                  resolver [name] (NameSpace cx) =
                                    case M.lookup name cx of
                                      Nothing -> Nothing
                                      Just (fqName,nameType,_) -> Just (encodeModuleNames fqName,nameType)
                                  resolver (name:rest) (NameSpace cx) =
                                    case M.lookup name cx of
                                      Nothing -> Nothing
                                      Just (_,_,Nothing) -> Nothing
                                      Just (_,_,Just cx') -> resolver rest cx'
                              in msum . map (resolver nameIn) $ context
      processFDP fdp = fdp
        { D.FileDescriptorProto.message_type=fmap (processMSG protoContext) (D.FileDescriptorProto.message_type fdp)
        , D.FileDescriptorProto.enum_type=fmap (processENM protoContext) (D.FileDescriptorProto.enum_type fdp)
        , D.FileDescriptorProto.service=fmap (processSRV protoContext) (D.FileDescriptorProto.service fdp)
        , D.FileDescriptorProto.extension=fmap (processFLD protoContext Nothing) (D.FileDescriptorProto.extension fdp) }
      processMSG cx msg = msg
        { D.DescriptorProto.name=self
        , D.DescriptorProto.field=fmap (processFLD cx' self) (D.DescriptorProto.field msg)
        , D.DescriptorProto.extension=fmap (processFLD cx' self) (D.DescriptorProto.extension msg)
        , D.DescriptorProto.nested_type=fmap (processMSG cx') (D.DescriptorProto.nested_type msg)
        , D.DescriptorProto.enum_type=fmap (processENM cx') (D.DescriptorProto.enum_type msg) }
       where cx' = descend cx (D.DescriptorProto.name msg)
             self = resolve cx (D.DescriptorProto.name msg)
      processFLD cx mp f = f { D.FieldDescriptorProto.name=mangleFieldName (D.FieldDescriptorProto.name f)
                             , D.FieldDescriptorProto.type'=(D.FieldDescriptorProto.type' f) `mplus` (fmap (t.snd) r2)
                             , D.FieldDescriptorProto.type_name=checkSelf mp (fmap fst r2)
                             , D.FieldDescriptorProto.extendee=resolve cx (D.FieldDescriptorProto.extendee f) }
       where r2 = resolve2 cx (D.FieldDescriptorProto.type_name f)
             t Message = TYPE_MESSAGE
             t Enumeration = TYPE_ENUM
             t Void = error "processFLD cannot resolve type_name to Void"
             checkSelf (Just parent) x@(Just name) = if parent==name then (D.FieldDescriptorProto.type_name f) else x
             checkSelf _ x = x
      processENM cx e = e { D.EnumDescriptorProto.name=resolve cx (D.EnumDescriptorProto.name e) }
      processSRV cx s = s { D.ServiceDescriptorProto.name=resolve cx (D.ServiceDescriptorProto.name s)
                          , D.ServiceDescriptorProto.method=fmap (processMTD cx) (D.ServiceDescriptorProto.method s) }
      processMTD cx m = m { D.MethodDescriptorProto.name=mangleFieldName (D.MethodDescriptorProto.name m)
                          , D.MethodDescriptorProto.input_type=resolve cx (D.MethodDescriptorProto.input_type m)
                          , D.MethodDescriptorProto.output_type=resolve cx (D.MethodDescriptorProto.output_type m) }
  in processFDP protoIn
