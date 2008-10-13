-- | This "Parser" module takes a filename and its contents as a
-- bytestring, and uses Lexer.hs to make a stream of tokens that it
-- parses. No IO is performed and the error function is not used.
-- Since the Lexer should also avoid such errors this should be a
-- reliably total function of the input.
--
-- The inernals have been updated to handle Google's protobuf-
module Text.ProtocolBuffers.ProtoCompile.Parser(parseProto) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D(ExtensionRange)
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.ExtensionRange(ExtensionRange(..))
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
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D.FieldDescriptorProto(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type         (Type(..))
import qualified Text.DescriptorProtos.FieldOptions                   as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions                   as D.FieldOptions(FieldOptions(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.MessageOptions                 as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions                 as D.MessageOptions(MessageOptions(..))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.MethodOptions                  as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions                  as D.MethodOptions(MethodOptions(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto)
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..))
import qualified Text.DescriptorProtos.ServiceOptions                 as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions                 as D.ServiceOptions(ServiceOptions(..))
import qualified Text.DescriptorProtos.UninterpretedOption            as D(UninterpretedOption)
import qualified Text.DescriptorProtos.UninterpretedOption            as D.UninterpretedOption(UninterpretedOption(..))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D(NamePart)
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D.NamePart(NamePart(..))

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header(ByteString,Int32,Int64,Word32,Word64
                                  ,ReflectEnum(reflectEnumInfo),enumName)
import Text.ProtocolBuffers.ProtoCompile.Lexer(Lexed(..),alexScanTokens,getLinePos)
import Text.ProtocolBuffers.ProtoCompile.Instances(parseLabel,parseType)

import Control.Monad(when,liftM2,liftM3,replicateM)
import qualified Data.ByteString.Lazy as L(unpack)
import qualified Data.ByteString.Lazy.Char8 as LC(notElem,head,uncons)
import qualified Data.ByteString.Lazy.UTF8 as U(fromString,toString,uncons)
import Data.Char(isUpper,toLower)
import Data.Ix(inRange)
import Data.Maybe(fromMaybe)
import Data.Sequence((|>))
import qualified Data.Sequence as Seq(fromList)
import Text.ParserCombinators.Parsec(GenParser,ParseError,runParser,sourceName,anyToken
                                    ,getInput,setInput,getPosition,setPosition,getState,setState
                                    ,(<?>),(<|>),token,choice,between,eof,unexpected,skipMany)
import Text.ParserCombinators.Parsec.Pos(newPos)
import Data.Word(Word8)

default ()

type P = GenParser Lexed

parseProto :: String -> ByteString -> Either ParseError D.FileDescriptorProto
parseProto filename file = do
  let lexed = alexScanTokens file
      ipos = case lexed of
               [] -> setPosition (newPos filename 0 0)
               (l:_) -> setPosition (newPos filename (getLinePos l) 0)
  runParser (ipos >> parser) (initState filename) filename lexed

utf8FromString :: String -> Maybe Utf8
utf8FromString = Just . Utf8 . U.fromString
utf8ToString :: Utf8 -> String
utf8ToString = U.toString . utf8

initState :: String -> D.FileDescriptorProto
initState filename = defaultValue {D.FileDescriptorProto.name=utf8FromString filename}

{-# INLINE mayRead #-}
mayRead :: ReadS a -> String -> Maybe a
mayRead f s = case f s of [(a,"")] -> Just a; _ -> Nothing

true,false :: ByteString
true = U.fromString "true"
false = U.fromString "false"

-- Use 'token' via 'tok' to make all the parsers for the Lexed values
tok :: (Lexed -> Maybe a) -> P s a
tok f = token show (\lexed -> newPos "" (getLinePos lexed) 0) f

pChar :: Char -> P s ()
pChar c = tok (\l-> case l of L _ x -> if (x==c) then return () else Nothing
                              _ -> Nothing) <?> ("character "++show c)

eol,eols :: P s ()
eol = pChar ';'
eols = skipMany eol

pName :: ByteString -> P s Utf8
pName name = tok (\l-> case l of L_Name _ x -> if (x==name) then return (Utf8 x) else Nothing
                                 _ -> Nothing) <?> ("name "++show (U.toString name))

bsLit :: P s ByteString
bsLit = tok (\l-> case l of L_String _ x -> return x
                            _ -> Nothing) <?> "quoted bytes literal"

strLit :: P s Utf8
strLit = tok (\l-> case l of L_String _ x -> case isValidUTF8 x of
                                               Nothing -> return (Utf8 x)
                                               Just n -> fail $ "bad utf-8 byte in string literal position # "++show n
                             _ -> fail "quoted string literal (UTF-8)")

intLit,fieldInt,enumInt :: (Num a) => P s a
intLit = tok (\l-> case l of L_Integer _ x -> return (fromInteger x)
                             _ -> Nothing) <?> "integer literal"

fieldInt = tok (\l-> case l of L_Integer _ x | inRange validRange x && not (inRange reservedRange x) -> return (fromInteger x)
                               _ -> Nothing) <?> "field number (from 0 to 2^29-1 and not in 19000 to 19999)"
  where validRange = (0,(2^(29::Int))-1)
        reservedRange = (19000,19999)

enumInt = tok (\l-> case l of L_Integer _ x | inRange validRange x -> return (fromInteger x)
                              _ -> Nothing) <?> "enum value (from -2^31 to 2^31-1)"
  where validRange = (toInteger (minBound :: Int32), toInteger (maxBound :: Int32))

doubleLit :: P s Double
doubleLit = tok (\l-> case l of L_Double _ x -> return x
                                L_Integer _ x -> return (fromInteger x)
                                _ -> Nothing) <?> "double (or integer) literal"

ident,ident1,ident_package,ident_strip :: P s Utf8
ident = tok (\l-> case l of L_Name _ x -> return (Utf8 x)
                            _ -> Nothing) <?> "identifier (perhaps dotted)"

ident1 = tok (\l-> case l of L_Name _ x | LC.notElem '.' x -> return (Utf8 x)
                             _ -> Nothing) <?> "identifier (not dotted)"

ident_package = tok (\l-> case l of L_Name _ x | LC.head x /= '.' -> return (Utf8 x)
                                    _ -> Nothing) <?> "package name (no leading dot)"

ident_strip = tok (\l-> case l of L_Name _ x -> case LC.uncons x of
                                                 (Just ('.',y)) -> return (Utf8 y)
                                                 _ -> Nothing
                                  _ -> Nothing) <?> "part of option name (with leading dot)"

boolLit :: P s Bool
boolLit = tok (\l-> case l of L_Name _ x | x == true -> return True
                                         | x == false -> return False
                              _ -> Nothing) <?> "boolean literal ('true' or 'false')"

enumLit :: forall s a. (Read a,ReflectEnum a) => P s a -- This is very polymorphic, and with a good error message
enumLit = do
  s <- fmap' utf8ToString ident1
  case mayRead reads s of
    Just x -> return x
    Nothing -> let self = enumName (reflectEnumInfo (undefined :: a))
               in unexpected $ "Enum value not recognized: "++show s++", wanted enum value of type "++show self

-- subParser changes the user state. It is a bit of a hack and is used
-- to define an interesting style of parsing below.
subParser :: Show t => GenParser t sSub a -> sSub -> GenParser t s sSub
subParser doSub inSub = do
  in1 <- getInput
  pos1 <- getPosition
  let out = runParser (setPosition pos1 >> doSub >> getStatus) inSub (sourceName pos1) in1
  case out of
    Left pe -> do
      context <- replicateM 5 anyToken
      fail ( unlines [ "The error message from the nested subParser was:\n"++indent (show pe) 
                     , "  The next 5 tokens were "++show context ] )
    Right (outSub,in2,pos2) -> setInput in2 >> setPosition pos2 >> return outSub
 where getStatus = liftM3 (,,) getState getInput getPosition
       indent = unlines . map (\s -> ' ':' ':s) . lines

{-# INLINE return' #-}
return' :: (Monad m) => a -> m a
return' a = return $! a

{-# INLINE fmap' #-}
fmap' :: (Monad m) => (a->b) -> m a -> m b
fmap' f m = m >>= \a -> seq a (return $! (f a))

{-# INLINE update' #-}
update' :: (s -> s) -> P s ()
update' f = getState >>= \s -> setState $! (f s)

parser :: P D.FileDescriptorProto D.FileDescriptorProto 
parser = proto >> getState
  where proto = eof <|> (choice [ eol
                                , importFile
                                , package
                                , fileOption
                                , message upTopMsg
                                , enum upTopEnum
                                , extend upTopMsg upTopExt
                                , service] >> proto)
        upTopMsg msg = update' (\s -> s {D.FileDescriptorProto.message_type=D.FileDescriptorProto.message_type s |> msg})
        upTopEnum e  = update' (\s -> s {D.FileDescriptorProto.enum_type=D.FileDescriptorProto.enum_type s |> e})
        upTopExt f   = update' (\s -> s {D.FileDescriptorProto.extension=D.FileDescriptorProto.extension s |> f})

importFile,package,fileOption,service :: P D.FileDescriptorProto.FileDescriptorProto ()
importFile = pName (U.fromString "import") >> strLit >>= \p -> eol >> update' (\s -> s {D.FileDescriptorProto.dependency=(D.FileDescriptorProto.dependency s) |> p})

package = pName (U.fromString "package") >> do
  p <- ident_package
  eol
  update' (\s -> s {D.FileDescriptorProto.package=Just p})

-- This parses the new extensible option name format of Google's protobuf verison 2.0.2
-- "foo.(bar.baz).qux" goes to Left [("foo",False),("bar.baz",True),("qux",False)]
pOptionE :: P s (Either D.UninterpretedOption String)
pOptionE = do
  let pieces = withParens <|> withoutParens False
      withParens = do
        part <- between (pChar '(') (pChar ')') ident
        fmap ((part,True) :) ( choice [ pChar '=' >> return []
                                      , pChar '.' >> withParens
                                      , withoutParens True] )
      withoutParens stripLeadingDot = do
        part <- if stripLeadingDot then ident_strip else ident
        fmap ((part,False):) ( choice [ pChar '=' >> return []
                                      , pChar '.' >> withParens ] )
  nameParts <- pieces
  case nameParts of
    [(optName,False)] -> return (Right (utf8ToString optName))
    _ -> do uno <- pUnValue (makeUninterpetedOption nameParts)
            return (Left uno)

pOptionWith :: P s t -> P s (Either D.UninterpretedOption String, t)
pOptionWith = liftM2 (,) (pName (U.fromString "option") >> pOptionE)

pUnValue :: D.UninterpretedOption -> P s D.UninterpretedOption
pUnValue uno = tok storeLexed where
  storeLexed (L_Name _ bs) = return $ uno {D.UninterpretedOption.identifier_value = Just (Utf8 bs)}
  storeLexed (L_Integer _ i) | i >= 0 =
    return $ uno { D.UninterpretedOption.positive_int_value = Just (fromInteger i) }
                             | otherwise =
    return $ uno { D.UninterpretedOption.negative_int_value = Just (fromInteger i) }
  storeLexed (L_Double _ d) = return $ uno {D.UninterpretedOption.double_value = Just d }
  storeLexed (L_String _ bs) = return $ uno {D.UninterpretedOption.string_value = Just bs }
  storeLexed _ = Nothing

makeUninterpetedOption :: [(Utf8,Bool)] -> D.UninterpretedOption
makeUninterpetedOption nameParts = defaultValue { D.UninterpretedOption.name = Seq.fromList . map makeNamePart $ nameParts }
  where makeNamePart (name_part,is_extension) = defaultValue { D.NamePart.name_part = name_part
                                                             , D.NamePart.is_extension =  is_extension }

fileOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.FileDescriptorProto.options) getState
  setNew p = update' (\s -> s {D.FileDescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' (old {D.FileOptions.uninterpreted_option = D.FileOptions.uninterpreted_option old |> uno})
  setOption (Right optName,old) =
    case optName of
      "java_package"         -> strLit  >>= \p -> return' (old {D.FileOptions.java_package        =Just p})
      "java_outer_classname" -> strLit  >>= \p -> return' (old {D.FileOptions.java_outer_classname=Just p})
      "java_multiple_files"  -> boolLit >>= \p -> return' (old {D.FileOptions.java_multiple_files =Just p})
      "optimize_for"         -> enumLit >>= \p -> return' (old {D.FileOptions.optimize_for        =Just p})
      _ -> unexpected $ "FileOptions has no option named " ++ optName

message :: (D.DescriptorProto -> P s ()) -> P s ()
message up = pName (U.fromString "message") >> do
  self <- ident1
  up =<< subParser (pChar '{' >> subMessage) (defaultValue {D.DescriptorProto.name=Just self})

-- subMessage is also used to parse group declarations
subMessage,messageOption,extensions :: P D.DescriptorProto.DescriptorProto ()
subMessage = (pChar '}') <|> (choice [ eol
                                     , field upNestedMsg Nothing >>= upMsgField
                                     , message upNestedMsg
                                     , enum upNestedEnum
                                     , extensions
                                     , extend upNestedMsg upExtField
                                     , messageOption] >> subMessage)
  where upNestedMsg msg = update' (\s -> s {D.DescriptorProto.nested_type=D.DescriptorProto.nested_type s |> msg})
        upNestedEnum e  = update' (\s -> s {D.DescriptorProto.enum_type=D.DescriptorProto.enum_type s |> e})
        upMsgField f    = update' (\s -> s {D.DescriptorProto.field=D.DescriptorProto.field s |> f})
        upExtField f    = update' (\s -> s {D.DescriptorProto.extension=D.DescriptorProto.extension s |> f})

messageOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.DescriptorProto.options) getState
  setNew p = update' (\s -> s {D.DescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' (old {D.MessageOptions.uninterpreted_option = D.MessageOptions.uninterpreted_option old |> uno })
  setOption (Right optName,old) =
    case optName of
      "message_set_wire_format" -> boolLit >>= \p -> return' (old {D.MessageOptions.message_set_wire_format=Just p})
      _ -> unexpected $ "MessageOptions has no option named "++optName

extend :: (D.DescriptorProto -> P s ()) -> (D.FieldDescriptorProto -> P s ()) -> P s ()
extend upGroup upField = pName (U.fromString "extend") >> do
  typeExtendee <- ident
  pChar '{'
  let rest = (field upGroup (Just typeExtendee) >>= upField) >> eols >> (pChar '}' <|> rest)
  eols >> rest

field :: (D.DescriptorProto -> P s ()) -> Maybe Utf8 -> P s D.FieldDescriptorProto
field upGroup maybeExtendee = do 
  let allowedLabels = case maybeExtendee of
                        Nothing -> ["optional","repeated","required"]
                        Just {} -> ["optional","repeated"] -- cannot declare a required extension
  sLabel <- choice . map (pName . U.fromString) $ allowedLabels
  theLabel <- maybe (fail ("not a valid Label :"++show sLabel)) return (parseLabel (utf8ToString sLabel))
  sType <- ident
  let (maybeTypeCode,maybeTypeName) = case parseType (utf8ToString sType) of
                                        Just t -> (Just t,Nothing)
                                        Nothing -> (Nothing, Just sType)
  name <- ident1
  number <- pChar '=' >> fieldInt
  let v1 = defaultValue { D.FieldDescriptorProto.name = Just name
                        , D.FieldDescriptorProto.number = Just number
                        , D.FieldDescriptorProto.label = Just theLabel
                        , D.FieldDescriptorProto.type' = maybeTypeCode
                        , D.FieldDescriptorProto.type_name = maybeTypeName
                        , D.FieldDescriptorProto.extendee = maybeExtendee }
  if maybeTypeCode == Just TYPE_GROUP
    then do let nameString = utf8ToString name
            when (null nameString) (fail "Impossible? ident1 for field name was empty")
            when (not (isUpper (head nameString))) (fail $ "Group names must start with an upper case letter: "++show name)
            upGroup =<< subParser (pChar '{' >> subMessage) (defaultValue {D.DescriptorProto.name=Just name})
            let fieldName = utf8FromString (map toLower nameString)  -- down-case the whole name
                v = v1 { D.FieldDescriptorProto.name = fieldName
                       , D.FieldDescriptorProto.type_name = Just name }
            return v
    else (eol >> return v1) <|> (subParser (pChar '[' >> subField maybeTypeCode) v1)
             
subField,defaultConstant :: Maybe Type -> P D.FieldDescriptorProto ()
subField mt = (defaultConstant mt <|> fieldOption mt) >> ( (pChar ']' >> eol) <|> (pChar ',' >> subField mt) )

defaultConstant mt = do
  pName (U.fromString "default")
  maybeDefault <- pChar '=' >> fmap Just (constant mt)
  -- XXX Hack: we lie about Utf8 for the default_value below
  update' (\s -> s { D.FieldDescriptorProto.default_value = fmap Utf8 maybeDefault })

-- This does a type and range safe parsing of the default value,
-- except for enum constants which cannot be checked (the definition
-- may not have been parsed yet).
--
-- Double and Float are checked to be not-Nan and not-Inf.  The
-- int-like types are checked to be within the corresponding range.
constant :: Maybe Type -> P s ByteString
constant Nothing = fmap utf8 ident1 -- hopefully a matching enum; forget about Utf8
constant (Just t) =
  case t of
    TYPE_DOUBLE  -> do d <- doubleLit
                       when (isNaN d || isInfinite d)
                            (fail $ "default floating point literal "++show d++" is out of range for type "++show t)
                       return' (U.fromString . show $ d)
    TYPE_FLOAT   -> do d <- doubleLit
                       let fl :: Float
                           fl = read (show d)
                       when (isNaN fl || isInfinite fl || (d==0) /= (fl==0))
                            (fail $ "default floating point literal "++show d++" is out of range for type "++show t)
                       return' (U.fromString . show $ d)
    TYPE_BOOL    -> boolLit >>= \b -> return' $ if b then true else false
    TYPE_STRING  -> bsLit >>= \s -> case isValidUTF8 s of Nothing -> (return s) 
                                                          Just errPos -> fail $ "default string literal is invalid UTF-8 at byte # "++show errPos
    TYPE_BYTES   -> bsLit
    TYPE_GROUP   -> unexpected $ "cannot have a default for field of "++show t
    TYPE_MESSAGE -> unexpected $ "cannot have a default for field of "++show t
    TYPE_ENUM    -> fmap utf8 ident1 -- IMPOSSIBLE : SHOULD HAVE HAD Maybe Type PARAMETER match Nothing
    TYPE_SFIXED32 -> f (undefined :: Int32)
    TYPE_SINT32   -> f (undefined :: Int32)
    TYPE_INT32    -> f (undefined :: Int32)
    TYPE_SFIXED64 -> f (undefined :: Int64)
    TYPE_SINT64   -> f (undefined :: Int64)
    TYPE_INT64    -> f (undefined :: Int64)
    TYPE_FIXED32  -> f (undefined :: Word32)
    TYPE_UINT32   -> f (undefined :: Word32)
    TYPE_FIXED64  -> f (undefined :: Word64)
    TYPE_UINT64   -> f (undefined :: Word64)
  where f :: (Bounded a,Integral a) => a -> P s ByteString
        f u = do let range = (toInteger (minBound `asTypeOf` u),toInteger (maxBound `asTypeOf` u))
                 i <- intLit
                 when (not (inRange range i))
                      (fail $ "default integer value "++show i++" is out of range for type "++show t)
                 return' (U.fromString . show $ i)

-- Returns Nothing if valid, and the position of the error if invalid
isValidUTF8 :: ByteString -> Maybe Int
isValidUTF8 ws = go 0 (L.unpack ws) 0 where
  go :: Int -> [Word8] -> Int -> Maybe Int
  go 0 [] _ = Nothing
  go 0 (x:xs) n | x <= 127 = go 0 xs $! succ n -- binary 01111111
                | x <= 193 = Just n            -- binary 11000001, decodes to <=127, should not be here
                | x <= 223 = go 1 xs $! succ n -- binary 11011111
                | x <= 239 = go 2 xs $! succ n -- binary 11101111
                | x <= 243 = go 3 xs $! succ n -- binary 11110011
                | x == 244 = high xs $! succ n -- binary 11110100
                | otherwise = Just n
  go i (x:xs) n | 128 <= x && x <= 191 = go (pred i) xs $! succ n
  go _ _ n = Just n
  -- leading 3 bits are 100, so next 6 are at most 001111, i.e. 10001111
  high (x:xs) n | 128 <= x && x <= 143 = go 2 xs $! succ n
                | otherwise = Just n
  high [] n = Just n

fieldOption :: Maybe Type -> P D.FieldDescriptorProto ()
fieldOption mt = liftM2 (,) pOptionE getOld >>= setOption >>= setNew where
  getOld = fmap (fromMaybe defaultValue . D.FieldDescriptorProto.options) getState
  setNew p = update' (\s -> s { D.FieldDescriptorProto.options = Just p })
  setOption (Left uno,old) =
    return' (old {D.FieldOptions.uninterpreted_option = D.FieldOptions.uninterpreted_option old |> uno })
  setOption (Right optName,old) =
    case optName of
      "ctype" | (Just TYPE_STRING) == mt -> do
        enumLit >>= \p -> return' (old {D.FieldOptions.ctype=Just p})
      "experimental_map_key" | Nothing == mt -> do
        strLit >>= \p -> return' (old {D.FieldOptions.experimental_map_key=Just p})
      _ -> unexpected $ "FieldOptions has no option named "++optName

enum :: (D.EnumDescriptorProto -> P s ()) -> P s ()
enum up = pName (U.fromString "enum") >> do
  self <- ident1
  up =<< subParser (pChar '{' >> subEnum) (defaultValue {D.EnumDescriptorProto.name=Just self})

subEnum,enumOption :: P D.EnumDescriptorProto.EnumDescriptorProto ()
subEnum = eols >> rest -- Note: Must check enumOption before enumVal
  where rest = (enumOption <|> enumVal) >> eols >> (pChar '}' <|> rest)

enumOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.EnumDescriptorProto.options) getState
  setNew p = update' (\s -> s {D.EnumDescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' $  (old {D.EnumOptions.uninterpreted_option = D.EnumOptions.uninterpreted_option old |> uno })
  setOption (Right optName,_old) =
    case optName of
      _ -> unexpected $ "EnumOptions has no option named "++optName

enumVal :: P D.EnumDescriptorProto ()
enumVal = do
  name <- ident1
  number <- pChar '=' >> enumInt
  let v1 = defaultValue { D.EnumValueDescriptorProto.name = Just name
                        , D.EnumValueDescriptorProto.number = Just number }
  v <- (eol >> return v1) <|> subParser (pChar '{' >> subEnumValue) v1
  update' (\s -> s {D.EnumDescriptorProto.value=D.EnumDescriptorProto.value s |> v})

subEnumValue,enumValueOption :: P D.EnumValueDescriptorProto ()
subEnumValue = pChar '}' <|> (choice [ eol, enumValueOption ] >> subEnumValue)

enumValueOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.EnumValueDescriptorProto.options) getState
  setNew p = update' (\s -> s {D.EnumValueDescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' $  (old {D.EnumValueOptions.uninterpreted_option = D.EnumValueOptions.uninterpreted_option old |> uno })
  setOption (Right optName,_old) =
    case optName of
      _ -> unexpected $ "EnumValueOptions has no option named "++optName

extensions = pName (U.fromString "extensions") >> do
  start <- fmap Just fieldInt
  pName (U.fromString "to")
  end <- (fmap Just fieldInt <|> fmap (const Nothing) (pName (U.fromString "max")))
  let e = defaultValue { D.ExtensionRange.start = start
                       , D.ExtensionRange.end = end }
  update' (\s -> s {D.DescriptorProto.extension_range=D.DescriptorProto.extension_range s |> e})

service = pName (U.fromString "service") >> do
  name <- ident1
  f <- subParser (pChar '{' >> subService) (defaultValue {D.ServiceDescriptorProto.name=Just name})
  update' (\s -> s {D.FileDescriptorProto.service=D.FileDescriptorProto.service s |> f})

 where subService = pChar '}' <|> (choice [ eol, rpc, serviceOption ] >> subService)

serviceOption,rpc :: P D.ServiceDescriptorProto ()
serviceOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.ServiceDescriptorProto.options) getState
  setNew p = update' (\s -> s {D.ServiceDescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' (old {D.ServiceOptions.uninterpreted_option = D.ServiceOptions.uninterpreted_option old |> uno })
  setOption (Right optName,_old) =
    case optName of
      _ -> unexpected $ "ServiceOptions has no option named "++optName

rpc = pName (U.fromString "rpc") >> do
  name <- ident1
  input <- between (pChar '(') (pChar ')') ident1
  pName (U.fromString "returns")
  output <- between (pChar '(') (pChar ')') ident1
  let m1 = defaultValue { D.MethodDescriptorProto.name=Just name
                      , D.MethodDescriptorProto.input_type=Just input
                      , D.MethodDescriptorProto.output_type=Just output }
  m <- (eol >> return m1) <|> subParser (pChar '{' >> subRpc) m1
  update' (\s -> s {D.ServiceDescriptorProto.method=D.ServiceDescriptorProto.method s |> m})

subRpc,rpcOption :: P D.MethodDescriptorProto ()
subRpc = pChar '}' <|> (choice [ eol, rpcOption ] >> subRpc)

rpcOption = pOptionWith getOld >>= setOption >>= setNew >> eol where
  getOld = fmap (fromMaybe defaultValue . D.MethodDescriptorProto.options) getState
  setNew p = update' (\s -> s {D.MethodDescriptorProto.options=Just p})
  setOption (Left uno,old) =
    return' $  (old {D.MethodOptions.uninterpreted_option = D.MethodOptions.uninterpreted_option old |> uno })
  setOption (Right optName,_old) =
    case optName of
      _ -> unexpected $ "MethodOptions has no option named "++optName

{-
-- see google's stubs/strutil.cc lines 398-449/1121 and C99 specification
-- This mainly targets three digit octal codes
cEncode :: [Word8] -> [Char]
cEncode = concatMap one where
  one :: Word8 -> [Char]
  one x | (32 <= x) && (x < 127) = [toEnum .  fromEnum $  x]  -- main case of unescaped value
  one 9 = sl  't'
  one 10 = sl 'n'
  one 13 = sl 'r'
  one 34 = sl '"'
  one 39 = sl '\''
  one 92 = sl '\\'
  one 0 = '\\':"000"
  one x | x < 8 = '\\':'0':'0':(showOct x "")
        | x < 64 = '\\':'0':(showOct x "")
        | otherwise = '\\':(showOct x "")
  sl c = ['\\',c]
-}
