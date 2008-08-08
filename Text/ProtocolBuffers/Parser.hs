module Text.ProtocolBuffers.Parser where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
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
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..)) 
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto(Label)
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label     as D.FieldDescriptorProto.Label(Label(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
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

import Text.ProtocolBuffers.Lexer(Lexed(..),alexScanTokens,getLinePos)
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Instances
import Text.ProtocolBuffers.Reflections
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Pos
import Data.Sequence((|>))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Control.Monad
import Data.Monoid

type P = GenParser Lexed

indent :: String -> String
indent = unlines . map (\s -> ' ':' ':s) . lines

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

filename2 = "/tmp/unittest.proto"
filename = "/tmp/descriptor.proto"

initState :: D.FileDescriptorProto
initState = mergeEmpty {D.FileDescriptorProto.name = Just (LC.pack filename)}

pbParse = do file <- L.readFile filename
             let lex = alexScanTokens file
                 ipos = case lex of
                          [] -> setPosition (newPos filename 0 0)
                          (l:_) -> setPosition (newPos filename (getLinePos l) 0)
             return $ runParser (ipos >> parser) initState filename lex

tok :: (Lexed -> Maybe a) -> P s a
tok f = token show (\lexed -> newPos "" (getLinePos lexed) 0) f

pChar :: Char -> P s ()
pChar c = tok (\ l-> case l of L _ x -> if (x==c) then return () else Nothing; _ -> Nothing) <?> ("character "++show c)

pNameBS :: L.ByteString -> P s L.ByteString
pNameBS name = tok (\ l-> case l of L_Name _ x -> if (x==name) then return x else Nothing; _ -> Nothing) <?> ("name "++show (LC.unpack name))

pName :: String -> P s L.ByteString
pName name = tok (\ l-> case l of L_Name _ x -> if (x==name') then return x else Nothing; _ -> Nothing) <?> ("name "++show name)
  where name' = LC.pack name


strLit :: P s L.ByteString
strLit = tok (\ l-> case l of L_String _ x -> return x; _ -> Nothing) <?> "quoted string literal"

intLit :: (Num a) => P s a
intLit = tok (\ l-> case l of L_Integer _ x -> return (fromInteger x); _ -> Nothing) <?> "integer literal"

doubleLit :: P s Double
doubleLit = tok (\ l-> case l of L_Double _ x -> return x; _ -> Nothing) <?> "double literal"

ident = tok (\ l-> case l of L_Name _ x -> return x; _ -> Nothing) <?> "identifier (perhaps dotted)"

ident1 = tok (\ l-> case l of L_Name _ x | LC.notElem '.' x -> return x; _ -> Nothing) <?> "identifier (not dotted)"

ident2 = tok (\ l-> case l of L_Name _ x | LC.head x /= '.' -> return x; _ -> Nothing) <?> "identifier (no leading dot)"

boolLit = tok (\ l-> case l of L_Name _ x | LC.unpack x == "true" -> return True
                               L_Name _ x | LC.unpack x == "false" -> return False
                               _ -> Nothing)
          <?> "boolean literal ('true' or 'false')"

a `eq` b = do a' <- a
              pChar '='
              b' <- b
              return (a',b')

eol = \a -> pChar ';' >> return a

updateState' :: (s -> s) -> GenParser t s ()
updateState' f = getState >>= \s -> setState $! (f s)

-- subParser changes the user state. It is a bit of a hack, but works well for me here
subParser :: GenParser t sSub a -> sSub -> GenParser t s sSub
subParser doSub inSub = do
  in1 <- getInput
  pos1 <- getPosition
  let out = runParser (setPosition pos1 >> doSub >> clean) inSub (sourceName pos1) in1
  case out of Left pe -> fail ("the error from the nested subParser was:\n"++indent (show pe))
              Right (outSub,in2,pos2) -> setInput in2 >> setPosition pos2 >> return outSub
 where
  clean = do
   in2 <- getInput
   pos2 <- getPosition
   outSub <- getState
   return (outSub,in2,pos2)

updateFDP :: (D.FileDescriptorProto -> D.FileDescriptorProto) -> P D.FileDescriptorProto ()
updateFDP = updateState'

updateMSG :: (D.DescriptorProto -> D.DescriptorProto) -> P D.DescriptorProto ()
updateMSG = updateState'

{-# INLINE return' #-}
return' :: (Monad m) => a -> m a
return' a = return $! a

enumLit :: forall s a. (Read a,ReflectEnum a) => P s a
enumLit = do
  s <- fmap LC.unpack ident1
  case mayRead reads s of
    Just x -> return x
    Nothing -> let self = enumName (reflectEnumInfo (undefined :: a))
               in unexpected $ "Enum value note recognized: "++show s++", wanted enum type "++show self

parser = proto >> eof >>getState

proto = many (importFile <|> package <|> fileOption <|> message) -- ( import | package | message | extend | enum | option | ";" )*

importFile = pName "import" >> strLit >>= eol >>= \p -> updateFDP (\s -> s {D.FileDescriptorProto.dependency = (D.FileDescriptorProto.dependency s) |> p})

package = pName "package" >> ident2 >>= eol >>= \p -> updateFDP (\s -> s {D.FileDescriptorProto.package = Just p})

fileOption = pName "option" >> setOption >>= eol >>= \p -> updateFDP (\s -> s {D.FileDescriptorProto.options = Just p})
  where
    setOption = do
      optName <- ident1
      pChar '='
      old <- fmap (maybe mergeEmpty id . D.FileDescriptorProto.options) getState
      case (LC.unpack optName) of
        "java_package"         -> strLit >>= \p -> return' (old {D.FileOptions.java_package=Just p})
        "java_outer_classname" -> strLit >>= \p -> return' (old {D.FileOptions.java_outer_classname=Just p})
        "java_multiple_files"  -> boolLit >>= \p -> return' (old {D.FileOptions.java_multiple_files=Just p})
        "optimize_for"         -> enumLit >>= \p -> return' (old {D.FileOptions.optimize_for=Just p})
        s -> unexpected $ "option name "++s

message :: P D.FileDescriptorProto ()
message = pName "message" >> do
  self <- ident1
  pChar '{'
  msg <- subParser pMessage (mergeEmpty {D.DescriptorProto.name = Just self})
  updateFDP (\s -> s {D.FileDescriptorProto.message_type=D.FileDescriptorProto.message_type s |> msg})

nestedMessage :: P D.DescriptorProto ()
nestedMessage = pName "message" >> do
  self <- ident1
  pChar '{'
  msg <- subParser pMessage (mergeEmpty {D.DescriptorProto.name = Just self})
  updateMSG (\s -> s {D.DescriptorProto.nested_type=D.DescriptorProto.nested_type s |> msg})

pMessage :: P D.DescriptorProto ()
pMessage = many (field Nothing <|> nestedMessage) >> pChar '}'

field maybeExtendee = do 
  sLabel <- choice . map pName $ ["optional","repeated","required"]
  label <- maybe (fail ("not a valid Label :"++show sLabel)) return (parseLabel (LC.unpack sLabel))
  sType <- ident
  let (typeCode,typeName) = case parseType (LC.unpack sType) of
                              Just t -> (Just t,Nothing)
                              Nothing -> (Nothing, Just sType)
  name <- ident1
  pChar '='
  number <- intLit
  (maybeOptions,maybeDefault) <- option (Nothing,Nothing) $ do
    pChar '['
    subParser (bracketOptions typeCode) (Nothing,Nothing)
  pChar ';'
  let f = D.FieldDescriptorProto.FieldDescriptorProto
               { D.FieldDescriptorProto.name = Just name
               , D.FieldDescriptorProto.number = Just number
               , D.FieldDescriptorProto.label = Just label
               , D.FieldDescriptorProto.type' = typeCode
               , D.FieldDescriptorProto.type_name = typeName
               , D.FieldDescriptorProto.extendee = maybeExtendee
               , D.FieldDescriptorProto.default_value = maybeDefault
               , D.FieldDescriptorProto.options = maybeOptions
               }
  updateMSG (\s -> s {D.DescriptorProto.field=D.DescriptorProto.field s |> f})

bracketOptions :: Maybe Type
               -> P (Maybe D.FieldOptions.FieldOptions, Maybe L.ByteString) ()
bracketOptions mt = (defaultValue <|> fieldOptions) >> (pChar ']' <|> bracketOptions mt)
  where defaultValue = do
          pNameBS (LC.pack "default")
          pChar '='
          x <- constant mt
          (a,_) <- getState
          setState $! (a,Just x)
        fieldOptions = do
          optName <- ident1
          pChar '='
          (mOld,def) <- getState
          let old = maybe mergeEmpty id mOld
          case (LC.unpack optName) of
            "ctype" | (Just TYPE_STRING) == mt -> do
              enumLit >>= \p -> let new = old {D.FieldOptions.ctype=Just p}
                                in seq new $ setState $! (Just new,def)
            "experimental_map_key" | Nothing == mt -> do
              strLit >>= \p -> let new = old {D.FieldOptions.experimental_map_key=Just p}
                               in seq new $ setState$! (Just new,def)

constant :: Maybe Type -> P s L.ByteString
constant Nothing = ident1 -- enum constant
constant (Just t) =
  case t of
    TYPE_DOUBLE  -> fmap (LC.pack . show) $ doubleLit
    TYPE_FLOAT   -> fmap (LC.pack . show) $ doubleLit
    TYPE_BOOL    -> boolLit >>= \b -> return $ if b then LC.pack "true"
                                                    else LC.pack "false"
    TYPE_STRING  -> strLit
    TYPE_BYTES   -> strLit
    TYPE_GROUP   -> error "TYPE_GROUP cannot have a constant literal"
    TYPE_MESSAGE -> error "TYPE_MESSAGE cannot have a constant literal"
    TYPE_ENUM    -> fmap (LC.pack . show) $ ident1 -- hopefully a matching enum
    _            -> fmap (LC.pack . show) $ intLit

{-

import ::= "import" strLit ";"

package ::= "package" ident ( "." ident )* ";"

option ::= "option" optionBody ";"

optionBody ::= ident ( "." ident )* "=" constant

message ::= "message" ident messageBody

extend ::= "extend" userType "{" ( field | group | ";" )* "}"

enum ::= "enum" ident "{" ( option | enumField | ";" )* "}"

enumField ::= ident "=" intLit ";"

service ::= "service" ident "{" ( option | rpc | ";" )* "}"

rpc ::= "rpc" ident "(" userType ")" "returns" "(" userType ")" ";"

messageBody ::= "{" ( field | enum | message | extend | extensions | group | option | ":" )* "}"

group ::= label "group" camelIdent "=" intLit messageBody

field ::= label type ident "=" intLit ( "[" fieldOption ( "," fieldOption )* "]" )? ";"
# tag number must be 2^29-1 or lower, not 0, and not 19000-19999 (reserved)

fieldOption ::= optionBody | "default" "=" constant

# extension numbers must not overlap with field or other extension numbers
extensions ::= "extensions" extension ( "," extension )* ";"

extension ::= intLit ( "to" ( intLit | "max" ) )?

label ::= "required" | "optional" | "repeated"

type ::= "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
       | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
       | "bool" | "string" | "bytes" | userType

# leading dot for identifiers means they're fully qualified
userType ::= "."? ident ( "." ident )*

constant ::= ident | intLit | floatLit | strLit | boolLit

ident ::= /[A-Za-z_][\w_]*/

# according to parser.cc, group names must start with a capital letter as a
# hack for backwards-compatibility
camelIdent ::= /[A-Z][\w_]*/

intLit ::= decInt | hexInt | octInt

decInt ::= /[1-9]\d*/

hexInt ::= /0[xX]([A-Fa-f0-9])+/

octInt ::= /0[0-7]+/

# allow_f_after_float_ is disabled by default in tokenizer.cc
floatLit ::= /\d+(\.\d+)?([Ee][\+-]?\d+)?/

boolLit ::= "true" | "false"

# contents must not contain unescaped quote character
strLit ::= quote ( hexEscape | octEscape | charEscape | /[^\0\n]/ )* quote

quote ::= /["']/

hexEscape ::= /\\[Xx][A-Fa-f0-9]{1,2}/

octEscape ::= /\\0?[0-7]{1,3}/

charEscape ::= /\\[abfnrtv\\\?'"]/
-}