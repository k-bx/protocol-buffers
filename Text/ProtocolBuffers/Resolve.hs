module Text.ProtocolBuffers.Resolve where

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

newlineBefore s = go where
  go [] = []
  go (x:xs) | x `elem` s = '\n':x:go xs
            | otherwise = x:go xs

indent = unlines . map (\s -> ' ':' ':s) . lines

encodeModuleNames [] = mempty
encodeModuleNames xs = U.fromString . foldr1 (\a b -> a ++ '.':b) $ xs

mangleModuleNames :: ByteString -> [String]
mangleModuleNames bs = map mangleModuleName . splitDot . U.toString $ bs 

mangleCap :: Maybe ByteString -> [String]
mangleCap = mangleModuleNames . fromMaybe mempty

mangleCap1 :: Maybe ByteString -> String
mangleCap1 = mangleModuleName . U.toString . fromMaybe mempty

splitDot :: String -> [String]
splitDot = unfoldr s where
  s ('.':xs) = s xs
  s [] = Nothing
  s xs = Just (span ('.'/=) xs)

mangleModuleName :: String -> String
mangleModuleName [] = "Empty'Name"
mangleModuleName ('_':xs) = "U'"++xs
mangleModuleName (x:xs) | isLower x = toUpper x : xs
mangleModuleName xs = xs

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
  return (Box (fdp,resolveNames fdp))

--resolveNames :: D.FileDescriptorProto -> Context
resolveNames protoIn =
  let prefix = mangleCap . msum $
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
      descend :: Context -> Maybe ByteString -> Context
      descend cx@(NameSpace n:_) name =
        case M.lookup mangled n of
          Just (_,_,Nothing) -> cx
          Just (_,_,Just ns1) -> ns1:cx
          x -> error $ "Name resolution failed:\n"++unlines (mangled : show x : seeContext cx)
       where mangled = mangleCap1 name
      resolve :: Context -> Maybe ByteString -> Maybe ByteString
      resolve context bsIn = fmap fst (resolve2 context bsIn)
      resolve2 :: Context -> Maybe ByteString -> Maybe (ByteString,NameType)
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
        , D.FileDescriptorProto.extension=fmap (processFLD protoContext) (D.FileDescriptorProto.extension fdp) }
      processMSG cx msg = msg
        { D.DescriptorProto.name=resolve cx (D.DescriptorProto.name msg)
        , D.DescriptorProto.field=fmap (processFLD cx') (D.DescriptorProto.field msg)
        , D.DescriptorProto.extension=fmap (processFLD cx') (D.DescriptorProto.extension msg)
        , D.DescriptorProto.nested_type=fmap (processMSG cx') (D.DescriptorProto.nested_type msg)
        , D.DescriptorProto.enum_type=fmap (processENM cx') (D.DescriptorProto.enum_type msg) }
       where cx' = descend cx (D.DescriptorProto.name msg)
      processENM cx e = e { D.EnumDescriptorProto.name=resolve cx (D.EnumDescriptorProto.name e) }
      processSRV cx s = s { D.ServiceDescriptorProto.name=resolve cx (D.ServiceDescriptorProto.name s) }
      processFLD cx f = f { D.FieldDescriptorProto.type'=(D.FieldDescriptorProto.type' f) `mplus` (fmap (t.snd) r2)
                          , D.FieldDescriptorProto.type_name=fmap fst r2
                          , D.FieldDescriptorProto.extendee=resolve cx (D.FieldDescriptorProto.extendee f) }
        where r2 = resolve2 cx (D.FieldDescriptorProto.type_name f)
              t Message = TYPE_MESSAGE
              t Enumeration = TYPE_ENUM
              t Void = error "processFLD cannot resolve type_name to Void"
  in processFDP

-- also reserved is "_"
reserved = ["case","class","data","default","deriving","do","else"
           ,"if","import","in","infix","infixl","infixr","instance"
           ,"let","module","newtype","of","then","type","where"]

{- Initial name space

  fromList [("DescriptorProtos",(["DescriptorProtos"],Void,Just (NameSpace (fromList 
               [("DescriptorProto",(["DescriptorProtos","DescriptorProto"],Message,Just (NameSpace (fromList
                   [("ExtensionRange",(["DescriptorProtos","DescriptorProto","ExtensionRange"],Message,Nothing))]))))
               ,("EnumDescriptorProto",(["DescriptorProtos","EnumDescriptorProto"],Message,Nothing))
               ,("EnumOptions",(["DescriptorProtos","EnumOptions"],Message,Nothing))
               ,("EnumValueDescriptorProto",(["DescriptorProtos","EnumValueDescriptorProto"],Message,Nothing))
               ,("EnumValueOptions",(["DescriptorProtos","EnumValueOptions"],Message,Nothing))
               ,("FieldDescriptorProto",(["DescriptorProtos","FieldDescriptorProto"],Message,Just (NameSpace (fromList
                   [("Label",(["DescriptorProtos","FieldDescriptorProto","Label"],Enumeration,Nothing))
                   ,("Type",(["DescriptorProtos","FieldDescriptorProto","Type"],Enumeration,Nothing))]))))
               ,("FieldOptions",(["DescriptorProtos","FieldOptions"],Message,Just (NameSpace (fromList
                   [("CType",(["DescriptorProtos","FieldOptions","CType"],Enumeration,Nothing))]))))
               ,("FileDescriptorProto",(["DescriptorProtos","FileDescriptorProto"],Message,Nothing))
               ,("FileOptions",(["DescriptorProtos","FileOptions"],Message,Just (NameSpace (fromList
                   [("OptimizeMode",(["DescriptorProtos","FileOptions","OptimizeMode"],Enumeration,Nothing))]))))
               ,("MessageOptions",(["DescriptorProtos","MessageOptions"],Message,Nothing))
               ,("MethodDescriptorProto",(["DescriptorProtos","MethodDescriptorProto"],Message,Nothing))
               ,("MethodOptions",(["DescriptorProtos","MethodOptions"],Message,Nothing))
               ,("ServiceDescriptorProto",(["DescriptorProtos","ServiceDescriptorProto"],Message,Nothing))
               ,("ServiceOptions",(["DescriptorProtos","ServiceOptions"],Message,Nothing))]))))]
-}
n = "NameSpace (fromList [(\"DescriptorProto\",([\"DescriptorProtos\",\"DescriptorProto\"],Message,Just (NameSpace (fromList [(\"ExtensionRange\",([\"DescriptorProtos\",\"DescriptorProto\",\"ExtensionRange\"],Message,Nothing))])))),(\"EnumDescriptorProto\",([\"DescriptorProtos\",\"EnumDescriptorProto\"],Message,Nothing)),(\"EnumOptions\",([\"DescriptorProtos\",\"EnumOptions\"],Message,Nothing)),(\"EnumValueDescriptorProto\",([\"DescriptorProtos\",\"EnumValueDescriptorProto\"],Message,Nothing)),(\"EnumValueOptions\",([\"DescriptorProtos\",\"EnumValueOptions\"],Message,Nothing)),(\"FieldDescriptorProto\",([\"DescriptorProtos\",\"FieldDescriptorProto\"],Message,Just (NameSpace (fromList [(\"Label\",([\"DescriptorProtos\",\"FieldDescriptorProto\",\"Label\"],Enumeration,Nothing)),(\"Type\",([\"DescriptorProtos\",\"FieldDescriptorProto\",\"Type\"],Enumeration,Nothing))])))),(\"FieldOptions\",([\"DescriptorProtos\",\"FieldOptions\"],Message,Just (NameSpace (fromList [(\"CType\",([\"DescriptorProtos\",\"FieldOptions\",\"CType\"],Enumeration,Nothing))])))),(\"FileDescriptorProto\",([\"DescriptorProtos\",\"FileDescriptorProto\"],Message,Nothing)),(\"FileOptions\",([\"DescriptorProtos\",\"FileOptions\"],Message,Just (NameSpace (fromList [(\"OptimizeMode\",([\"DescriptorProtos\",\"FileOptions\",\"OptimizeMode\"],Enumeration,Nothing))])))),(\"MessageOptions\",([\"DescriptorProtos\",\"MessageOptions\"],Message,Nothing)),(\"MethodDescriptorProto\",([\"DescriptorProtos\",\"MethodDescriptorProto\"],Message,Nothing)),(\"MethodOptions\",([\"DescriptorProtos\",\"MethodOptions\"],Message,Nothing)),(\"ServiceDescriptorProto\",([\"DescriptorProtos\",\"ServiceDescriptorProto\"],Message,Nothing)),(\"ServiceOptions\",([\"DescriptorProtos\",\"ServiceOptions\"],Message,Nothing))])"

