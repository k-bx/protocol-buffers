module Text.ProtocolBuffers.ParseProto where

-- see compiler/parser.cc in the google source code

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
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto.Type(Type(..))
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

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Instances
import Text.ProtocolBuffers.Option

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language(emptyDef,LanguageDef(..))
import qualified Text.ParserCombinators.Parsec.Token as T (makeTokenParser,TokenParser(..))

import Data.List(intersperse)
import Data.Maybe(maybe)
import Data.Maybe(maybe)
import Data.Monoid
import System.IO(readFile)
import Data.ByteString.Char8(pack)

import qualified Data.Sequence as Seq

--t1 = mappend (Just (D.FileOptions.OptimizeMode.SPEED)) (Just (D.FileOptions.OptimizeMode.CODE_SIZE))

p = T.makeTokenParser protoDef

-- see google's io/tokenizer.{h,c}
protoDef = emptyDef {
             commentStart = []
           , commentEnd = []
           -- commentLine is really useful
           , commentLine = "//"
           , nestedComments = False
           -- This ident* agrees with comments in io/tokenizer.h
           , identStart = letter
           , identLetter = alphaNum <|> char '_'
           , opStart = pzero
           , opLetter = pzero
           -- The following reservedNames and reservedOpNames maybe ought to be [] and []
           , reservedNames = ["option","message"
                             ,"required","options","repeated"
                             ,"double","float"
                             ,"int32","int64"
                             ,"uint32","uint64"
                             ,"sint32","sint64"
                             ,"fixed32","fixed64"
                             ,"sfixed32","sfixed64"
                             ,"bool","string","bytes"
                             ]
           , reservedOpNames = ["="]
           , caseSensitive = True
  }

skip = T.whiteSpace p
sym = T.symbol p
semi = T.semi p
ident = T.identifier p <?> "Expecting identifier (letter then letter or number or _)"

type US = ()

-- Parsing a proto file produces a stream of items
data TopLevel = P !ByteString
              | D !ByteString 
              | M D.DescriptorProto
              | E D.EnumDescriptorProto
              | S D.ServiceDescriptorProto
              | F D.FieldDescriptorProto
              | O D.FileOptions
  deriving Show

unique :: OptionFlag a => String -> [b] -> (Option a b,Maybe String)
unique e [] = (Absent,Nothing)
unique e [b] = (Present b,Nothing)
unique e bs = (Present (last bs),Just e)

fileDescriptorProto :: CharParser US D.FileDescriptorProto
fileDescriptorProto = do
  skipTop ()
  val <- many (topLevel >>= skipTop)
--   -- Alternative to enforce having only a single package directive
--   let (opPackage,mayErr) = unique "Multiple package definitions found, only one expected."
--                                 [ x | P x <- val ]
--   maybe (return ()) fail mayErr
  return (defaultValue { -- D.FileDescriptorProto.package = opPackage
                         D.FileDescriptorProto.package      = mconcat [ Present x | P x <- val ]
                       , D.FileDescriptorProto.dependency   = Seq.fromList [ x | D x <- val ]
                       , D.FileDescriptorProto.message_type = Seq.fromList [ x | M x <- val ]
                       , D.FileDescriptorProto.enum_type    = Seq.fromList [ x | E x <- val ]
                       , D.FileDescriptorProto.service      = Seq.fromList [ x | S x <- val ]
                       , D.FileDescriptorProto.extension    = Seq.fromList [ x | F x <- val ]
                       , D.FileDescriptorProto.options      = mconcat [ Present x | O x <- val ]
                       } )

skipTop :: a -> CharParser US a
skipTop x = skip >> skipMany (semi >> skip) >> return x -- ignore empty declarations

topLevel :: CharParser US TopLevel
topLevel =  choice [ messageTop
                   , enumTop 
                   , serviceTop
                   , extendTop
                   , importTop
                   , packageTop
                   , optionTop
                   ] <?> "Expected top-level statement (e.g. \"message\")."

messageTop = string "message" >> do
  skip
  name <- ident
  return (M (defaultValue { D.DescriptorProto.name = Present (pack name) }))
 
enumTop = pzero
serviceTop = pzero
extendTop = pzero
importTop = pzero
packageTop = string "package" >> do
  skip
  parts <- sepBy1 ident (char '.') <?> "Expect package name (a dotted itentifier, e.g. beep.giggle.whee)"
  skip >> semi
  return (P (pack (foldr1 (\a b -> a ++ '.':b) parts)))

-- Hard code optionTop  avoid only use of reflection in google's parser
-- This can be make reflective after bootstrapping if this program is ever finished
optionTop = string "option" >> do
  skip
  optionName <- ident
  skip
  char '='
  skip
  opt <- optionByName optionName
  skip >> semi
  return (O opt)
 where
   optionByName "java_package" = do
     bs <- parseString
     return (defaultValue { D.FileOptions.java_package = Present bs })
   optionByName "java_outer_classname" = do
     bs <- parseString
     return (defaultValue { D.FileOptions.java_outer_classname = Present bs })
   optionByName "java_multiple_files" = do
     bool <- parseBool
     return (defaultValue { D.FileOptions.java_multiple_files = Present bool })
   optionByName "optimize_for" = do
     mode <- parseOptimizeMode
     return (defaultValue { D.FileOptions.optimize_for = Present mode })

parseString = pzero
parseBool = pzero
parseOptimizeMode = pzero

test f = do
  proto <- readFile f
  print $ parse fileDescriptorProto f proto

testTmp = fmap (parse fileDescriptorProto f) (readFile f)
  where f = "/tmp/test"

