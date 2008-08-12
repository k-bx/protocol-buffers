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

newtype NameSpace = NameSpace (Map String ([String],NameType,Maybe NameSpace))
  deriving (Show,Read)
data NameType = Message | Enumeration | Void
  deriving (Show,Read)

type Context = [NameSpace]
type Resolver = Context -> ByteString -> ByteString

--resolveNames :: D.FileDescriptorProto -> Context
resolveNames protoIn =
  let prefix = mangleCap . msum $
                 [ D.FileOptions.java_outer_classname =<< (D.FileDescriptorProto.options protoIn)
                 , D.FileOptions.java_package =<< (D.FileDescriptorProto.options protoIn)
                 , D.FileDescriptorProto.package protoIn]
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
      protoContext = foldl' (\nss@(NameSpace ns:_) pre -> case M.lookup pre ns of
                                                            Just (_,Void,Just ns1) -> (ns1:nss)
                                                            _ -> nss) [nameSpace] prefix
      resolve :: Context -> Maybe ByteString -> Maybe (ByteString,NameType)
      resolve context bsIn = let nameIn = mangleCap bsIn
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
  in resolve protoContext

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