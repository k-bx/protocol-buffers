module Text.ProtocolBuffers.ProtoCompile.ResolveM where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto(EnumDescriptorProto))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto(FieldDescriptorProto))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.Type(Type(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto(FileDescriptorProto))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.FileOptions                    as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions                    as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto)
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..))
import qualified Text.DescriptorProtos.UninterpretedOption            as D(UninterpretedOption)
import qualified Text.DescriptorProtos.UninterpretedOption            as D.UninterpretedOption(UninterpretedOption(..))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D(NamePart(NamePart))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D.NamePart(NamePart(..))

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.ProtoCompile.Instances
import Text.ProtocolBuffers.ProtoCompile.Parser

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Data.Char
import Data.Ix(inRange)
import Data.List(unfoldr,span,inits,foldl',stripPrefix)
import Data.Map(Map)
import Data.Maybe(fromMaybe,catMaybes,mapMaybe)
import Data.Monoid(Monoid(..))
import Data.Set(Set)
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Traversable as T

-- Used by err and throw
errMsg :: String -> String
errMsg s = "Text.ProtocolBuffers.ResolveM fatal error encountered, message:\n"++indent s
  where indent = unlines . map (\str -> ' ':' ':str) . lines

err :: forall b. String -> b
err = error . errMsg 

throw :: (MonadError String m) =>  String -> m a
throw s = throw (errMsg s)

just :: Typeable a => String -> Maybe a -> a
just s ma@Nothing = err $ "Impossible? Expected Just of type "++show (typeOf ma)++"but got nothing:\n  "++s
just s (Just a) = a

getJust :: (MonadError String m, Typeable a) => String -> Maybe a -> m a
{-#  INLINE getJust #-}
getJust s ma@Nothing = throw $ "Impossible? Expected Just of type "++show (typeOf ma)++"but got nothing:\n  "++s
getJust s (Just a) = return a

-- insert '.' between each item and convert to utf8 bytes
encodeModuleNames :: [String] -> Utf8
encodeModuleNames [] = Utf8 mempty
encodeModuleNames xs = Utf8 . U.fromString . foldr1 (\a b -> a ++ '.':b) $ xs

-- up-case the first letter after each '.', Nothing -> ""
mangleCap :: Maybe Utf8 -> [String]
mangleCap = mangleModuleNames . fromMaybe (Utf8 mempty)
  where mangleModuleNames :: Utf8 -> [String]
        mangleModuleNames bs = map mangleModuleName . splitDot $ bs

splitDot :: Utf8 -> [String]
splitDot = unfoldr s . toString where
  s ('.':xs) = s xs
  s [] = Nothing
  s xs = Just (span ('.'/=) xs)

-- By construction in the Lexer.x file, the use of parseIdent here cannot go wrong.
-- But I will be paranoid and write a total function anyway.
-- Call to 'parts' are with a non-empty string that should not start with a '.'
parseIdent :: (MonadError String m) => Utf8 -> m (Bool,[String])
{-#  INLINE parseIdent #-}
parseIdent bs = case LC.uncons (utf8 bs) of
                  Nothing -> throw "parseIdent: Invalid empty Utf8"
                  Just ('.',bs') | LC.null bs' -> throw "parseIdent: Invalid Utf8 of a single '.'"
                                 | otherwise -> liftM ((,) True) (parts (toString bs))
                  _ -> liftM ((,) False) (parts (toString bs))
  where parts s = case span ('.'/=) s of
                    ("", "") -> throw $ "parseIdent: Invalid Utf8 because it ends in a '.': "++show (toString bs)
                    ("",  _) -> throw $ "parseIdent: Invalid Utf8 because it contains two '.' in a row: "++show (toString bs)
                    ( x, "") -> return (x : [])
                    ( x,_:y) -> liftM (x :) (parts y)

joinDot :: [String] -> String
joinDot [] = ""
joinDot xs = concatMap ('.':) xs

-- up-case the first letter after each '.', Nothing -> Nothing
mangleCaps :: Maybe Utf8 -> String
mangleCaps Nothing = ""
mangleCaps x = foldr1 (\a b -> a ++ '.':b) (mangleCap x)

-- up-case the first letter, Nothing -> Nothing
mangleCap1 :: Maybe Utf8 -> String
mangleCap1 Nothing = ""
mangleCap1 (Just u) = mangleModuleName . toString $ u

-- up-case the first letter of each value name
mangleEnums :: Seq D.EnumValueDescriptorProto -> Seq D.EnumValueDescriptorProto
mangleEnums s =  fmap fixEnum s
  where fixEnum v = v { D.EnumValueDescriptorProto.name = mangleEnum (D.EnumValueDescriptorProto.name v)}

-- up-case the first letter
mangleEnum :: Maybe Utf8 -> Maybe Utf8
mangleEnum = fmap (Utf8 . U.fromString . mangleModuleName . toString)

-- up-case the first letter
mangleModuleName :: String -> String
mangleModuleName [] = "Empty'Name" -- XXX
mangleModuleName ('_':xs) = "U'"++xs
mangleModuleName (x:xs) | isLower x = let x' = toUpper x
                                      in if isLower x' then err ("subborn lower case"++show (x:xs))
                                           else x': xs
mangleModuleName xs = xs

-- down-case the first letter, add single quote if it is a reserved word
mangleFieldName :: Maybe Utf8 -> Maybe Utf8
mangleFieldName = fmap (Utf8 . U.fromString . fixname . toString)
  where fixname [] = "empty'name" -- XXX
        fixname ('_':xs) = "u'"++xs
        fixname (x:xs) | isUpper x = let x' = toLower x
                                     in if isUpper x' then err ("stubborn upper case: "++show (x:xs))
                                          else fixname (x':xs)
        fixname xs | xs `elem` reserved = xs ++ "'"
        fixname xs = xs
        reserved :: [String]
        reserved = ["case","class","data","default","deriving","do","else","foreign"
                   ,"if","import","in","infix","infixl","infixr","instance"
                   ,"let","module","newtype","of","then","type","where"] -- also reserved is "_"

checkER :: [(Int32,Int32)] -> Int32 -> Bool
checkER ers fid = any (`inRange` fid) ers

checkFI :: [(FieldId,FieldId)] -> FieldId -> Bool
checkFI ers fid = any (`inRange` fid) ers

extRangeList :: D.DescriptorProto -> [(Int32,Int32)]
extRangeList d = concatMap check unchecked
  where check x@(lo,hi) | hi < lo = []
                        | hi<19000 || 19999<lo  = [x]
                        | otherwise = concatMap check [(lo,18999),(20000,hi)]
        unchecked = F.foldr ((:) . extToPair) [] (D.DescriptorProto.extension_range d)
        extToPair (D.ExtensionRange
                    { D.ExtensionRange.start = start
                    , D.ExtensionRange.end = end }) =
          (getFieldId $ maybe minBound FieldId start, getFieldId $ maybe maxBound FieldId end)

getExtRanges :: D.DescriptorProto -> [(FieldId,FieldId)]
getExtRanges d = concatMap check unchecked
  where check x@(lo,hi) | hi < lo = []
                        | hi<19000 || 19999<lo  = [x]
                        | otherwise = concatMap check [(lo,18999),(20000,hi)]
        unchecked = F.foldr ((:) . extToPair) [] (D.DescriptorProto.extension_range d)
        extToPair (D.ExtensionRange
                    { D.ExtensionRange.start = start
                    , D.ExtensionRange.end = end }) =
          (maybe minBound FieldId start, maybe maxBound FieldId end)

newtype NameSpace = NameSpace {unNameSpace::(Map String ([String],NameType,Maybe NameSpace))}
  deriving (Show,Read)
data NameType = Message [(Int32,Int32)] | Enumeration [Utf8] | Service | Void 
  deriving (Show,Read)
type Context = [NameSpace]

-- | By construction Env is 0 or more Local Entity namespaces followed
-- by 1 or more Global TopLevel namespaces (self and imported files).
-- Entities in first Global TopLevel namespace can refer to each other
-- and to Entities in the list of directly imported TopLevel namespaces only.
data Env = Local Entity {- E'Message -} Env | Global TopLevel [TopLevel]  deriving Show
-- | TopLevel corresponds to all items defined in a .proto file. This
-- includes the FileOptions since this will be consulted when
-- generating the Haskel module names, and the imported files are only
-- known through their TopLevel data.
data TopLevel = TopLevel FilePath [String] (Maybe D.FileOptions) EMap {- E'Message,Enum,Key,Service -} deriving Show
-- | The EMap type is a local namespace attached to an entity
type EMap = Map String Entity
-- | An Entity is some concrete item in the namespace of a proto file.
-- All Entity values have a leading-dot fully-qualified with the package "eName".
-- The E'Message,Group,Service have EMap namespaces to inner Entity items.
data Entity = E'Message { eName :: [String], validExtensions :: [(FieldId,FieldId)]
                                           , mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }
            | E'Group   { eName :: [String], mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }
            | E'Field   { eName :: [String], fNumber :: FieldId, fType :: Maybe D.Type
                                           , mVal :: Maybe (Either String Entity) {- E'Message,Group,Enum -} }
            | E'Key     { eName :: [String], eMsg :: Either String Entity         {- E'Message -}
                                           , fNumber :: FieldId, fType :: Maybe D.Type
                                           , mVal :: Maybe (Either String Entity) {- E'Message,Group,Enum -} }
            | E'Enum    { eName :: [String], eVals :: Set Utf8 }
            | E'Service { eName :: [String], mVals :: EMap {- E'Method -} }
            | E'Method  { eName :: [String], eMsgIn,eMsgOut :: Maybe (Either String Entity) {- E'Message -} }
            | E'Error String [Entity]
  deriving (Show)

type E a = Env -> Either String a

fqName :: Entity -> Utf8
fqName = Utf8 . U.fromString . joinDot . eName

get'mVals :: Entity -> Maybe EMap
get'mVals (E'Message {mVals = x}) = Just x
get'mVals (E'Group {mVals = x}) = Just x
get'mVals (E'Service {mVals = x}) = Just x
get'mVals _ = Nothing

-- | This is a helper for resolveEnv
toGlobal (Local _ env) = toGlobal env
toGlobal x@(Global {}) = x

-- | This is used for resolving some UninterpretedOption names
resolveHere :: Utf8 -> RE Entity
resolveHere nameU = do
  let rFail msg = throw ("Could not lookup "++show (toString nameU)++"\n  "++msg)
  (isGlobal,xs) <- parseIdent nameU
  when isGlobal $ rFail "because only local name expected (no leading '.')"
  envIn <- ask
  case xs of
    [x] -> case envIn of
             Local parent _ -> case get'mVals parent of
                                 Just vals -> case M.lookup x vals of
                                                Just entity -> return entity
                                                Nothing -> rFail ("because there is no such name here:\n  "++whereEnv envIn)
                                 Nothing -> rFail ("because environment has no local names:\n  "++whereEnv envIn)
             Global {} -> rFail ("because the environment is global: "++whereEnv envIn)
    _ -> rFail "because only a simple name expected (no internal '.'s)"

-- | 'resolveEnv' is the query operation for the Env namespace.  It
-- recorgnizes names beginning with a '.' as already being
-- fully-qualified names. This is called from the different monads via
-- resolveRE, resolveSE, or getType.
resolveEnv :: Utf8 -> E Entity
resolveEnv nameU envIn = do
  (isGlobal,xs) <- parseIdent nameU
  let mResult = if isGlobal then lookupEnv xs (toGlobal envIn)
                            else lookupEnv xs envIn
  case mResult of
    Nothing -> throw ("Could not lookup "++show (toString nameU)++" in environment: "++(whereEnv envIn))
    Just e@(E'Error {}) -> throw (show e)
    Just e -> return e


resolveRE :: Utf8 -> RE Entity
resolveRE nameU = lift . (resolveEnv nameU) =<< ask

resolveSE :: Utf8 -> SE (Either String Entity)
resolveSE nameU = fmap (resolveEnv nameU) (asks snd)

-- | 'getType' is used to lookup the type strings in service method records.
getType :: Show a => String -> (a -> Maybe Utf8) -> a -> SE (Maybe (Either String Entity))
getType s f a = do
  typeU <- getJust s (f a)
  case parseType (toString typeU) of
    Just _ -> return Nothing
    Nothing -> do ee <- resolveSE typeU
                  return (Just (expectMGE ee))

-- | 'expectMGE' is used by getType and 'entityField'
expectMGE :: Either String Entity -> Either String Entity
expectMGE ee@(Left {}) = ee
expectMGE ee@(Right e) = if isMGE e then ee
                           else Left $ "expectMGE: Name resolution failed to find a Message, Group, or Enum:\n  "++show e
  where isMGE e = case e of E'Message {} -> True
                            E'Group {} -> True
                            E'Enum {} -> True
                            _ -> False

-- | 'expectM' is used by 'entityField'
expectM :: Either String Entity -> Either String Entity
expectM ee@(Left {}) = ee
expectM ee@(Right e) = if isMGE e then ee
                         else Left $ "expectMGE: Name resolution failed to find a Message, Group, or Enum:\n  "++show e
  where isMGE e = case e of E'Message {} -> True
                            _ -> False

-- | This is a helper for resolveEnv for error messages
whereEnv :: Env -> String
whereEnv (Local entity env) = joinDot (eName entity) ++ " in "++show ( (\(Global (TopLevel filePath _ _ _ ) _)-> filePath) (toGlobal env))
whereEnv (Global (TopLevel filePath names _ _) _) = joinDot names ++ " in " ++ show filePath

-- | lookupEnv is used only by resolveEnv
lookupEnv :: [String] -> Env -> Maybe Entity
lookupEnv xs (Global tl tls) = lookupTopLevel xs tl <|> msum (map (lookupTopLevel xs) tls)
lookupEnv xs (Local e@(E'Error {}) _env) = return e
lookupEnv xs (Local entity env) = case get'mVals entity of
                                    Just vals -> lookupVals vals xs <|> lookupEnv xs env
                                    Nothing -> Nothing

-- | lookupTopLeve is used only by lookupEnv
lookupTopLevel :: [String] -> TopLevel -> Maybe Entity
lookupTopLevel xs (TopLevel _filePath names _fileOptions vals) = stripPrefix names xs >>= lookupVals vals

-- | lookupVals is used by lookupEnv and lookupTopLevel
lookupVals :: EMap -> [String] -> Maybe Entity
lookupVals vals [] = Nothing
lookupVals vals [x] = M.lookup x vals
lookupVals vals (x:xs) = do entity <-  M.lookup x vals
                            case get'mVals entity of
                              Just vals' -> lookupVals vals' xs
                              Nothing -> Nothing

-- | 'partEither' separates the Left errors and Right success in the obvious way.
partEither :: [Either a b] -> ([a],[b])
partEither [] = ([],[])
partEither (Left a:xs) = let ~(ls,rs) = partEither xs
                         in (a:ls,rs)
partEither (Right b:xs) = let ~(ls,rs) = partEither xs
                          in (ls,b:rs)

-- | The 'unique' function is used with Data.Map.fromListWithKey to detect
-- name collisions and record this as E'Error entries in the map.
unique :: String -> Entity -> Entity -> Entity
unique name (E'Error _ a) (E'Error _ b) = E'Error ("Namespace collision for "++name) (a++b)
unique name (E'Error _ a) b = E'Error ("Namespace collision for "++name) (a++[b])
unique name a (E'Error _ b) = E'Error ("Namespace collision for "++name) (a:b)
unique name a b = E'Error ("Namespace collision for "++name) [a,b]

-- | 'makeTopLevel' takes a .proto file's FileDescriptorProto and the
-- TopLevel values of its directly imported file and constructs the
-- TopLevel of the FileDescriptorProto in a Global Environment.
--
-- This goes to some lengths to be a total function with good error
-- messages.  Erros in building the skeleton of the namespace are
-- detected and reported instead of returning the new 'Global'
-- environment.  Collisions in the namespace are only detected when
-- the offending name is lookedup, it will return an E'Error entity
-- with a message and list of colliding Entity items.  The
-- cross-linking of Entity fields may fail and this failure is stored
-- in the corresponding Entity.
--
-- Also caught: name collisions in Enum definitions.
makeTopLevel :: D.FileDescriptorProto -> [TopLevel] -> Either String Env {- Global -}
makeTopLevel fdp imports = do
  filePath <- getJust "makeTopLevel.filePath" (D.FileDescriptorProto.name fdp)
  packageName <- fmap snd . parseIdent =<< getJust "makeTopLevel.packageName" (D.FileDescriptorProto.package fdp)
  let global = Global topLevel imports
      topLevel = TopLevel (toString filePath) packageName (D.FileDescriptorProto.options fdp) (M.fromListWithKey unique children)
      children = concat [ messages, keys, enums, services ]
      (badMessages,messages) = partEither . map (runSE packageName global . entityMsg (const False))
                             . F.toList . D.FileDescriptorProto.message_type $ fdp
      (badKeys,keys) = partEither . map (runSE packageName global .entityField True)
                     . F.toList . D.FileDescriptorProto.extension $ fdp
      (badEnums,enums) = partEither . map (runSE packageName global . entityEnum)
                       . F.toList . D.FileDescriptorProto.enum_type $ fdp
      bad = unlines . concat $ [ badMessages, badKeys, badEnums, badSevices ]
      (badSevices,services) = partEither . map (runSE packageName global . entityService)
                            . F.toList . D.FileDescriptorProto.service $ fdp
  when (not (null bad)) $
    throw $ "makeTopLevel.bad: Some children failed for "++show filePath++"\n"++bad
  return global

type RE a = ReaderT Env (Either String) a

type SE a = ReaderT ([String],Env) (Either String) a

maybeM :: Monad m => (x -> m a) -> (Maybe x) -> m (Maybe a)
maybeM f mx = maybe (return Nothing) (liftM Just . f) mx

runSE :: [String] -> Env -> SE a -> Either String a
runSE a b = flip runReaderT (a,b)

getNames :: String -> (a -> Maybe Utf8) -> a -> SE (String,[String])
getNames s f a = do names <- asks fst
                    name <- fmap toString $ getJust s (f a)
                    return (name,names++[name])

kids :: (a -> SE (String,Entity)) -> Seq a -> SE ([String],[(String,Entity)])
kids f xs = do (a,b) <- ask
               return . partEither . map (runSE a b) . map f . F.toList $ xs

makeTopLevel' :: D.FileDescriptorProto -> [TopLevel] -> Either String Env {- Global -}
makeTopLevel' fdp imports = mdo
  filePath <- getJust "makeTopLevel.filePath" (D.FileDescriptorProto.name fdp)
  packageName <- fmap snd . parseIdent =<< getJust "makeTopLevel.packageName" (D.FileDescriptorProto.package fdp)
  global <- flip runReaderT (packageName,global) $ do
    (bads,children) <- fmap unzip . sequence $
      [ kids (entityMsg (const False)) (D.FileDescriptorProto.message_type fdp)
      , kids (entityField True)        (D.FileDescriptorProto.extension    fdp)
      , kids entityEnum                (D.FileDescriptorProto.enum_type    fdp)
      , kids entityService             (D.FileDescriptorProto.service      fdp) ]
    let global' = Global (TopLevel (toString filePath) packageName (D.FileDescriptorProto.options fdp) (M.fromListWithKey unique (concat children))) imports
        bad = unlines (concat bads)
    when (not (null bad)) $
      throw $ "makeTopLevel.bad: Some children failed for "++show filePath++"\n"++bad
    return global'
  return global

{- ***

All the entity* functions are used by makeTopLevel and each other.
They are very scrupulous in being total functions, there is no use of
'error' or 'undefined' and all failures (many of which are Impossible)
are reported by hopefully sensible (Left String) messages.

 *** -}

entityMsg :: (String -> Bool) -> D.DescriptorProto -> SE (String,Entity)
entityMsg isGroup dp = mdo
  (self,names) <- getNames "entityMsg.name" D.DescriptorProto.name dp
  numbers <- fmap Set.fromList . mapM (getJust "entityMsg.field.number" . D.FieldDescriptorProto.number) . F.toList . D.DescriptorProto.field $ dp
  when (Set.size numbers /= Seq.length (D.DescriptorProto.field dp)) $
    throwError $ "entityMsg.field.number: There must be duplicate field numbers for "++show names++"\n "++show numbers
  let groupNames = map toString . mapMaybe D.FieldDescriptorProto.type_name
                 . filter (maybe False (TYPE_GROUP ==) . D.FieldDescriptorProto.type') 
                 . F.toList . D.DescriptorProto.field $ dp
  entity <- local (\(_,env) -> (names,Local entity env)) $ do
    (bads,children) <- fmap unzip . sequence $
      [ kids entityEnum                      (D.DescriptorProto.enum_type   dp)
      , kids (entityField True)              (D.DescriptorProto.extension   dp)
      , kids (entityField False)             (D.DescriptorProto.field       dp)
      , kids (entityMsg (`elem` groupNames)) (D.DescriptorProto.nested_type dp) ]
    let entity' | isGroup self = E'Group names (M.fromListWithKey unique (concat children))
                | otherwise = E'Message names (getExtRanges dp) (M.fromListWithKey unique (concat children))
        bad = unlines (concat bads)
    when (not (null bad)) $
      throwError $ "entityMsg.bad: Some children failed for "++show names++"\n"++bad
    return entity'
  return (self,entity)

entityField :: Bool -> D.FieldDescriptorProto -> SE (String,Entity)
entityField isKey fdp = do
  (self,names) <- getNames "entityField.name" D.FieldDescriptorProto.name fdp
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    throwError $ "entityField: Impossible? Expected key and got field or vice-versa:\n  "++show ((isKey,isKey'),names,fdp)
  number <- getJust "entityField.name" . D.FieldDescriptorProto.number $ fdp
  let mType = D.FieldDescriptorProto.type' fdp
  typeName <- maybeM (fmap expectMGE . resolveSE) (D.FieldDescriptorProto.type_name fdp)
  if isKey then do extendee <- fmap expectM . resolveSE =<< getJust "entityField.extendee" (D.FieldDescriptorProto.extendee fdp)
                   return (self,E'Key names extendee (FieldId number) mType typeName)
           else return (self,E'Field names (FieldId number) mType typeName)

entityEnum :: D.EnumDescriptorProto -> SE (String,Entity)
entityEnum edp@(D.EnumDescriptorProto {D.EnumDescriptorProto.value=vs}) = do
  (self,names) <- getNames "entityEnum.name" D.EnumDescriptorProto.name edp
  values <- fmap Set.fromList . mapM (getJust "entityEnum.value.number" . D.EnumValueDescriptorProto.number) . F.toList $ vs
  when (Set.size values /= Seq.length vs) $
    throwError $ "entityEnum.value.number: There must be duplicate enum values for "++show names++"\n "++show values
  valNames <- fmap Set.fromList . mapM (getJust "entityEnum.value.name" . D.EnumValueDescriptorProto.name) . F.toList $ vs
  when (Set.size valNames /= Seq.length vs) $
    throwError $ "entityEnum.value.name: There must be duplicate enum names for "++show names++"\n "++show (map toString (Set.toList valNames))
  return (self,E'Enum names valNames) -- discard values

entityService :: D.ServiceDescriptorProto -> SE (String,Entity)
entityService sdp = mdo
  (self,names) <- getNames "entityService.name" D.ServiceDescriptorProto.name sdp
  let entity = E'Service names (M.fromListWithKey unique methods)
  (badMethods,methods) <- local (\(_,env) -> (names,env)) $
                          kids entityMethod (D.ServiceDescriptorProto.method sdp)
  when (not (null badMethods)) $
    throwError $ "entityService.badMethods: Some methods failed for "++show names++"\n"++unlines badMethods
  return (self,entity)

entityMethod :: D.MethodDescriptorProto -> SE (String,Entity)
entityMethod mdp = do
  (self,names) <- getNames "entityMethod.name" D.MethodDescriptorProto.name mdp
  inputType <- getType "entityMethod.input_type" D.MethodDescriptorProto.input_type mdp
  outputType <- getType "entityMethod.output_type" D.MethodDescriptorProto.output_type mdp
  return (self,E'Method names inputType outputType)

{- ***

The namespace Env is used to transform the original
FileDescriptorProto into a canonical FileDescriptorProto. The new
goal is to match the transformation done by Google's protoc program.

This should be a total function with no 'error' or 'undefined' values
possible.

*** -}

fqFail :: Show a => String -> a -> Entity -> RE b
fqFail msg dp entity = do
  env <- ask
  throw $ unlines [ msg, "resolving: "++show dp, "in environment: "++whereEnv env, "found: "++show entity ]

resolveFDP :: D.FileDescriptorProto -> Env -> Either String D.FileDescriptorProto
resolveFDP = runReaderT . fqFileDP 

fqFileDP :: D.FileDescriptorProto -> RE D.FileDescriptorProto
fqFileDP fdp = do
  newMessages <- T.mapM fqMessage      (D.FileDescriptorProto.message_type fdp)
  newEnums    <- T.mapM fqEnum         (D.FileDescriptorProto.enum_type    fdp)
  newServices <- T.mapM fqService      (D.FileDescriptorProto.service      fdp)
  newKeys     <- T.mapM (fqField True) (D.FileDescriptorProto.extension    fdp)
  return $ fdp { D.FileDescriptorProto.message_type = newMessages
               , D.FileDescriptorProto.enum_type    = newEnums
               , D.FileDescriptorProto.service      = newServices
               , D.FileDescriptorProto.extension    = newKeys }

fqMessage :: D.DescriptorProto -> RE D.DescriptorProto
fqMessage dp = do
  entity <- resolveRE =<< getJust "fqMessage.name" (D.DescriptorProto.name dp)
  case entity of
    E'Message {} -> return ()
    E'Group {} -> return ()
    _ -> fqFail "fqMessage.entity: did not resolve to an E'Message or E'Group:" dp entity
  newFields   <- T.mapM (fqField False) (D.DescriptorProto.field       dp)
  newKeys     <- T.mapM (fqField True)  (D.DescriptorProto.extension   dp)
  newMessages <- T.mapM fqMessage       (D.DescriptorProto.nested_type dp)
  newEnums    <- T.mapM fqEnum          (D.DescriptorProto.enum_type   dp)
  return $ dp { D.DescriptorProto.field       = newFields
              , D.DescriptorProto.extension   = newKeys
              , D.DescriptorProto.nested_type = newMessages
              , D.DescriptorProto.enum_type   = newEnums }

fqService :: D.ServiceDescriptorProto -> RE D.ServiceDescriptorProto
fqService sdp = do
  entity <- resolveRE =<< getJust "fqService.name" (D.ServiceDescriptorProto.name sdp)
  case entity of
    E'Service {} -> do newMethods <- local (Local entity) $ T.mapM fqMethod (D.ServiceDescriptorProto.method sdp)
                       return $ sdp { D.ServiceDescriptorProto.method = newMethods }
    _ -> fqFail "fqService.entity: did not resolve to a service:" sdp entity

fqMethod :: D.MethodDescriptorProto -> RE D.MethodDescriptorProto
fqMethod mdp = do
  entity <- resolveRE =<< getJust "fqMethod.name" (D.MethodDescriptorProto.name mdp)
  case entity of
    E'Method {} -> do mdp1 <- case eMsgIn entity of
                                Nothing -> return mdp
                                Just resolveIn -> do new <- fmap fqName (lift resolveIn)
                                                     return (mdp {D.MethodDescriptorProto.input_type = Just new})
                      mdp2 <- case eMsgIn entity of
                                Nothing -> return mdp1
                                Just resolveIn -> do new <- fmap fqName (lift resolveIn)
                                                     return (mdp1 {D.MethodDescriptorProto.input_type = Just new})
                      return mdp2
    _ -> fqFail "fqMethod.entity: did not resolve to a Method:" mdp entity

fqField :: Bool -> D.FieldDescriptorProto -> RE D.FieldDescriptorProto
fqField isKey fdp = do
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    ask >>= \env -> throwError $ "fqField.isKey: Expected key and got field or vice-versa:\n  "++show ((isKey,isKey'),whereEnv env,fdp)
  entity <- resolveRE =<< getJust "fqField.name" (D.FieldDescriptorProto.name fdp)
  newExtendee <- case (isKey,entity) of
                   (True,E'Key {}) -> do
                      ext <- lift (eMsg entity)
                      case ext of
                        E'Message {} -> when (not (checkFI (validExtensions ext) (fNumber entity))) $
                          throwError $ "fqField.newExtendee: Field Number of extention key invalid:\n"
                            ++unlines ["Number is "++show (fNumber entity),"Valid ranges: "++show (validExtensions ext),"Extendee: "++show (eName ext),"Descriptor: "++show fdp]
                        _ -> fqFail "fqField.ext: Key's target is not an E'Message:" fdp ext
                      fmap (Just . fqName) . lift . eMsg $ entity
                   (False,E'Field {}) -> return Nothing
                   _ -> fqFail "fqField.entity: did not resolve to expected E'Key or E'Field:" fdp entity
  mTypeName <- maybeM lift (mVal entity) -- "Just (Left _)" triggers a throwError here
  -- Finally fully determine D.Type, (type'==Nothing) meant ambiguously TYPE_MESSAGE or TYPE_ENUM from Parser.hs
  actualType <- case (fType entity,mTypeName) of
                  (Just TYPE_GROUP, Just (E'Group {})) -> return TYPE_GROUP
                  (Nothing, Just (E'Message {})) -> return TYPE_MESSAGE
                  (Nothing, Just (E'Enum {})) -> return TYPE_ENUM
                  (Just t, Nothing) -> return t
                  (mt,me) -> fqFail ("fqField.actualType: "++show mt++" and "++show (fmap eName me)++" is invalid") fdp entity
  -- Check that a default value of an TYPE_ENUM is valid
  case (mTypeName,D.FieldDescriptorProto.default_value fdp) of
    (Just ee@(E'Enum {eVals = enumVals}),Just enumVal) -> when (Set.notMember enumVal enumVals) $
      throwError $ "fqField.default_value: Default enum value is invalid:\n"
        ++unlines ["Value is "++show (toString enumVal),"Allowed values from "++show (eName ee)," are "++show enumVals,"Descriptor: "++show fdp]
    _ -> return ()
  if isKey then return (fdp { D.FieldDescriptorProto.extendee  = newExtendee
                            , D.FieldDescriptorProto.type'     = Just actualType
                            , D.FieldDescriptorProto.type_name = fmap fqName mTypeName })
           else return (fdp { D.FieldDescriptorProto.type'     = Just actualType
                            , D.FieldDescriptorProto.type_name = fmap fqName mTypeName })

fqEnum :: D.EnumDescriptorProto -> RE D.EnumDescriptorProto
fqEnum edp = do
  entity <- resolveRE =<< getJust "fqEnum.name" (D.EnumDescriptorProto.name edp)
  case entity of
    E'Enum {} -> return edp
    _ -> fqFail "fqEnum.entity: did not resolve to an E'Enum:" edp entity

{-

Before I retrofit the Haskell namespace changes, let me validate the
design for resolving new options.

Adding resolution calls during entity* phase, then access during fq*
phase was valid for creating the cross links, but the uninterpreted
options need to follow such trail, not make them.

Adding a new phase makes little sense, since each option is lookup up
separately and the fq* phase can handle the traversal.

Make the resolution during the fq* phase.  Each resolution needs
access to the "ExtMessage" instance and the Seq of
UninterpretedOption.

The exact record stored in the ExtendMessage will be a map association of
(FieldId,ExtFromWire WireType (Seq ByteString)) where the ByteString
is the WirePut of the corresponding information.

-}

interpretOptions :: ExtendMessage msg => msg -> Seq D.UninterpretedOption -> RE msg
interpretOptions msg unos = do
  ios <- mapM interpretOption . F.toList $ unos
  let (ExtField ef) = getExtField msg
      ef' = foldl' (\m (k,v) -> seq v $ M.insert k v m) ef ios
      msg' = seq ef' (putExtField (ExtField ef') msg)
  return msg'

interpretOption :: D.UninterpretedOption -> RE (FieldId,ExtFieldValue)
interpretOption uno = do
  let iFail msg = do env <- ask
                     throw $ unlines [ "interpretOption: Failed to handle UninterpretedOption"
                                     , "  environment: "++whereEnv env
                                     , "  value: "++show uno
                                     , "  message: "++msg ]
      go (D.NamePart { D.NamePart.name_part = name
                     , D.NamePart.is_extension = isKey }) [] = do
        entity <- if isKey then resolveRE name else resolveHere name
        case entity of
          E'Field {} | not isKey -> return ()
          E'Key {} | isKey -> return ()
          _ -> iFail $ "Last entity was resolved but was not an E'Field or E'Key: "++show entity
        -- XXX add more data to field and key to allow encoding of information, link to proto? Need typecode and field number
        -- ensure data type is a base type
        -- encode data type via wirePut
      go (D.NamePart { D.NamePart.name_part = name
                     , D.NamePart.is_extension = isKey }) (next:rest) = do
        case isKey of
          True -> do
            entity <- resolveRE name
            case entity of
              E'Key {} -> do undefined
                -- Check extendee-entity-eName is same as go's name
                -- Check data type is Just an E'Message or E'Group
                -- recurse go with data type's eName
                -- wrap result with proper start tag and length prefix or start/stop tags, need field number
              _ -> iFail $ "Name "++show (toString name)++" was resolved by was not an E'Key: "++show entity
          False -> do
            entity <- resolveHere name
            -- Check the entity is an E'Message or E'Group
            -- recurse go with data type's eName
            -- wrap result with proper start tag and length prefix or start/stop tags, need field number
            return undefined
  thing <- case F.toList (D.UninterpretedOption.name uno) of
             [] -> iFail $ "Empty name_part"
             (part:parts) -> go part parts
  return (0,undefined)

{-

New name resolution strategy:

1) create FDS that mimics protoc (CANONICAL)

Flow looks like
* load the proto file
** go off and load the imported files, get a Context from each
** build a Context for the current file
* 

2) namespace the keys and fields
3) resolve the (Seq UninterpretedOption.NamePart) references
4) mangling pass no longer applied to Text.DescriptorProto*, only by MakeReflection.
5) Expose Mangling API in protocol-buffers library

Master Map of CANONICAL names to Entity

-}

seeContext :: Context -> [String] 
seeContext cx = map ((++"[]") . concatMap (\k -> show k ++ ", ") . M.keys . unNameSpace) cx

toString :: Utf8 -> String
toString = U.toString . utf8

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile paths target = do
  let test [] = return Nothing
      test (path:rest) = do
        let fullname = combine path target
        found <- doesFileExist fullname
        if found then return (Just fullname)
          else test rest
  test paths

-- loadProto is a slight kludge.  It takes a single search directory
-- and an initial .proto file path relative to this directory.  It
-- loads this file and then chases the imports.  If an import loop is
-- detected then it aborts.  A state monad is used to memorize
-- previous invocations of 'load'.  A progress message of the filepath
-- is printed before reading a new .proto file.
--
-- The "contexts" collected and used to "resolveWithContext" can
-- contain duplicates: File A imports B and C, and File B imports C
-- will cause the context for C to be included twice in contexts.
--
-- The result type of loadProto is enough for now, but may be changed
-- in the future.  It returns a map from the files (relative to the
-- search directory) to a pair of the resolved descriptor and a set of
-- directly imported files.  The dependency tree is thus implicit.
loadProto :: [FilePath] -> FilePath -> IO (Map FilePath (D.FileDescriptorProto,Set.Set FilePath,[String]))
loadProto protoDirs protoFile = fmap answer $ execStateT (load Set.empty protoFile) mempty where
  answer built = fmap snd built -- drop the fst Context from the pair in the memorized map
  loadFailed f msg = fail . unlines $ ["Parsing proto:",f,"has failed with message",msg]
  load :: Set.Set FilePath  -- set of "parents" that is used by load to detect an import loop. Not memorized.
       -> FilePath          -- the FilePath to load and resolve (may used memorized result of load)
       -> StateT (Map FilePath (Context,(D.FileDescriptorProto,Set.Set FilePath,[String]))) -- memorized results of load
                 IO (Context  -- Only used during load. This is the view of the file as an imported namespace.
                    ,(D.FileDescriptorProto  -- This is the resolved version of the FileDescriptorProto
                     ,Set.Set FilePath
                     ,[String]))  -- This is the list of file directly imported by the FilePath argument
  load parentsIn file = do
    built <- get -- to check memorized results
    when (Set.member file parentsIn)
         (loadFailed file (unlines ["imports failed: recursive loop detected"
                                   ,unlines . map show . M.assocs $ built,show parentsIn]))
    let parents = Set.insert file parentsIn
    case M.lookup file built of
      Just result -> return result
      Nothing -> do
        mayToRead <- liftIO $ findFile protoDirs file
        case mayToRead of
          Nothing -> loadFailed file (unlines (["loading failed, could not find file: "++show file
                                               ,"Searched paths were:"] ++ map ("  "++) protoDirs))
          Just toRead -> do
            proto <- liftIO $ do print ("Loading filepath: "++toRead)
                                 LC.readFile toRead
            parsed <- either (loadFailed toRead . show) return (parseProto toRead proto)
            let (context,imports,names) = toContext parsed
            contexts <- fmap (concatMap fst)    -- keep only the fst Context's
                        . mapM (load parents)   -- recursively chase imports
                        . Set.toList $ imports
            let result = ( withPackage context parsed ++ contexts
                         , ( resolveWithContext (context++contexts) parsed
                           , imports
                           , names ) )
            -- add to memorized results, the "load" above may have updated/invalidated the "built <- get" state above
            modify (\built' -> M.insert file result built')
            return result

type Stuff = (D.FileDescriptorProto,Env,D.FileDescriptorProto)

asImport :: Stuff -> TopLevel
asImport (_,env,_) = let (Global topLevel _) = toGlobal env in topLevel

loadProto' :: [FilePath] -> FilePath -> IO Stuff
loadProto' protoDirs protoFile = evalStateT (load Set.empty protoFile) mempty where
  loadFailed f msg = fail . unlines $ ["Parsing proto:",f,"has failed with message",msg]
  load :: Set.Set FilePath -> FilePath -> StateT (Map FilePath Stuff) IO Stuff
  load parentsIn file = do
    built <- get
    when (Set.member file parentsIn)
         (loadFailed file (unlines ["imports failed: recursive loop detected"
                                   ,unlines . map show . M.assocs $ built,show parentsIn]))
    let parents = Set.insert file parentsIn
    case M.lookup file built of
      Just result -> return result
      Nothing -> do
        mayToRead <- liftIO $ findFile protoDirs file
        case mayToRead of
          Nothing -> loadFailed file (unlines (["loading failed, could not find file: "++show file
                                               ,"Searched paths were:"] ++ map ("  "++) protoDirs))
          Just toRead -> do
            proto <- liftIO $ do print ("Loading filepath: "++toRead)
                                 LC.readFile toRead
            parsed'fdp <- either (loadFailed toRead . show) return (parseProto toRead proto)
            let importList = map toString . F.toList . D.FileDescriptorProto.dependency $ parsed'fdp
            imports <- mapM (fmap asImport . load parents) importList
            global'env <- either (loadFailed file) return (makeTopLevel parsed'fdp imports)
            resolved'fdp <- either (loadFailed file) return (resolveFDP parsed'fdp global'env)
            let result = (parsed'fdp, global'env, resolved'fdp)
            -- add to memorized results, the "load" above may have updated/invalidated the "built <- get" state above
            modify (\built' -> M.insert file result built')
            return result

-- Imported names must be fully qualified in the .proto file by the
-- target's package name, but the resolved name might be fully
-- quilified by something else (e.g. one of the java options).
withPackage :: Context -> D.FileDescriptorProto -> Context
withPackage (nsIn:_) (D.FileDescriptorProto {D.FileDescriptorProto.package=Just package}) =
  let prepends = mangleCap . Just $ package
  in [foldr (\name nsRest -> NameSpace (M.singleton name ([name],Void,Just nsRest))) nsIn prepends]
withPackage (_:_) (D.FileDescriptorProto {D.FileDescriptorProto.name=n}) =  err $
  "withPackage given an imported FDP without a package declaration: "++show n
withPackage [] (D.FileDescriptorProto {D.FileDescriptorProto.name=n}) =  err $
  "withPackage given an empty context: "++show n

-- process to get top level context for FDP and list of its imports
toContext :: D.FileDescriptorProto -> (Context,Set.Set FilePath,[String])
toContext protoIn =
  let prefix :: [String]
      prefix = splitDot (just "toContext.prefix" (D.FileDescriptorProto.package protoIn))
      -- Make top-most root NameSpace
      nameSpace = fromMaybe (NameSpace mempty) $ foldr addPrefix protoNames $ zip prefix (tail (inits prefix))
        where addPrefix (s1,ss) ns = Just . NameSpace $ M.singleton s1 (ss,Void,ns)
              protoNames | null protoMsgs = Nothing
                         | otherwise = Just . NameSpace . M.fromList $ protoMsgs
                where protoMsgs = F.foldr ((:) . msgNames prefix) protoEnums (D.FileDescriptorProto.message_type protoIn)
                      protoEnums = F.foldr ((:) . enumNames prefix) protoServices (D.FileDescriptorProto.enum_type protoIn)
                      protoServices = F.foldr ((:) . serviceNames prefix) [] (D.FileDescriptorProto.service protoIn)
                      msgNames context dIn =
                        let s1 = mangleCap1 (D.DescriptorProto.name dIn)
                            ss' = context ++ [s1]
                            dNames | null dMsgs = Nothing
                                   | otherwise = Just . NameSpace . M.fromList $ dMsgs
                            dMsgs = F.foldr ((:) . msgNames ss') dEnums (D.DescriptorProto.nested_type dIn)
                            dEnums = F.foldr ((:) . enumNames ss') [] (D.DescriptorProto.enum_type dIn)
                        in ( s1 , (ss',Message (extRangeList dIn),dNames) )
                      enumNames context eIn = -- XXX todo mangle enum names ? No
                        let s1 = mangleCap1 (D.EnumDescriptorProto.name eIn)
                            values :: [Utf8]
                            values = catMaybes $ map D.EnumValueDescriptorProto.name (F.toList (D.EnumDescriptorProto.value eIn))
                        in ( s1 , (context ++ [s1],Enumeration values,Nothing) )
                      serviceNames context sIn =
                        let s1 = mangleCap1 (D.ServiceDescriptorProto.name sIn)
                        in ( s1 , (context ++ [s1],Service,Nothing) )
      -- Context stack for resolving the top level declarations
      protoContext :: Context
      protoContext = foldl' (\nss@(NameSpace ns:_) pre -> case M.lookup pre ns of
                                                            Just (_,Void,Just ns1) -> (ns1:nss)
                                                            _ -> nss) [nameSpace] prefix
  in ( protoContext
     , Set.fromList (map toString (F.toList (D.FileDescriptorProto.dependency protoIn)))
     , prefix
     )

resolveWithContext :: Context -> D.FileDescriptorProto -> D.FileDescriptorProto
resolveWithContext protoContext protoIn =
  let rerr msg = err $ "Failure while resolving file descriptor proto whose name is "
                       ++ maybe "<empty name>" toString (D.FileDescriptorProto.name protoIn)++"\n"
                       ++ msg
      descend :: Context -> Maybe Utf8 -> Context -- XXX todo take away the maybe 
      descend cx@(NameSpace n:_) name =
        case M.lookup mangled n of
          Just (_,_,Nothing) -> cx
          Just (_,_,Just ns1) -> ns1:cx
          x -> rerr $ "*** Name resolution failed when descending:\n"++unlines (mangled : show x : "KNOWN NAMES" : seeContext cx)
       where mangled = mangleCap1 name -- XXX empty on nothing?
      descend [] _ = []
      resolve :: Context -> Maybe Utf8 -> Maybe Utf8
      resolve _context Nothing = Nothing
      resolve context (Just bs) = fmap fst (resolveWithNameType context bs)
      resolveWithNameType :: Context -> Utf8 -> Maybe (Utf8,NameType)
      resolveWithNameType context bsIn =
        let nameIn = mangleCap (Just bsIn)
            errMsg = "*** Name resolution failed:\n"
                     ++unlines ["Unmangled name: "++show bsIn
                               ,"Mangled name: "++show nameIn
                               ,"List of known names:"]
                     ++ unlines (seeContext context)
            resolver [] (NameSpace _cx) = rerr $ "Impossible? case in Text.ProtocolBuffers.Resolve.resolveWithNameType.resolver []\n" ++ errMsg
            resolver [name] (NameSpace cx) = case M.lookup name cx of
                                               Nothing -> Nothing
                                               Just (fqName,nameType,_) -> Just (encodeModuleNames fqName,nameType)
            resolver (name:rest) (NameSpace cx) = case M.lookup name cx of
                                                    Nothing -> Nothing
                                                    Just (_,_,Nothing) -> Nothing
                                                    Just (_,_,Just cx') -> resolver rest cx'
        in case msum . map (resolver nameIn) $ context of
             Nothing -> rerr errMsg
             Just x -> Just x
      processFDP fdp = fdp
        { D.FileDescriptorProto.message_type = fmap (processMSG protoContext) (D.FileDescriptorProto.message_type fdp)
        , D.FileDescriptorProto.enum_type    = fmap (processENM protoContext) (D.FileDescriptorProto.enum_type fdp)
        , D.FileDescriptorProto.service      = fmap (processSRV protoContext) (D.FileDescriptorProto.service fdp)
        , D.FileDescriptorProto.extension    = fmap (processFLD protoContext Nothing) (D.FileDescriptorProto.extension fdp) }
      processMSG cx msg = msg
        { D.DescriptorProto.name        = self
        , D.DescriptorProto.field       = fmap (processFLD cx' self) (D.DescriptorProto.field msg)
        , D.DescriptorProto.extension   = fmap (processFLD cx' self) (D.DescriptorProto.extension msg)
        , D.DescriptorProto.nested_type = fmap (processMSG cx') (D.DescriptorProto.nested_type msg)
        , D.DescriptorProto.enum_type   = fmap (processENM cx') (D.DescriptorProto.enum_type msg) }
       where cx' = descend cx (D.DescriptorProto.name msg)
             self = resolve cx (D.DescriptorProto.name msg)
      processFLD cx mp f = f { D.FieldDescriptorProto.name          = mangleFieldName (D.FieldDescriptorProto.name f)
                             , D.FieldDescriptorProto.type'         = new_type'
                             , D.FieldDescriptorProto.type_name     = if new_type' == Just TYPE_GROUP
                                                                        then groupName
                                                                        else fmap fst r2
                             , D.FieldDescriptorProto.default_value = checkEnumDefault
                             , D.FieldDescriptorProto.extendee      = fmap newExt (D.FieldDescriptorProto.extendee f)}
       where newExt :: Utf8 -> Utf8
             newExt orig = let e2 = resolveWithNameType cx orig
                           in case (e2,D.FieldDescriptorProto.number f) of
                                (Just (newName,Message ers),Just fid) ->
                                  if checkER ers fid then newName
                                    else rerr $ "*** Name resolution found an extension field that is out of the allowed extension ranges: "++show f ++ "\n has a number "++ show fid ++" not in one of the valid ranges: " ++ show ers
                                (Just _,_) -> rerr $ "*** Name resolution found wrong type for "++show orig++" : "++show e2
                                (Nothing,Just {}) -> rerr $ "*** Name resolution failed for the extendee: "++show f
                                (_,Nothing) -> rerr $ "*** No field id number for the extension field: "++show f
             r2 = fmap (fromMaybe (rerr $ "*** Name resolution failed for the type_name of extension field: "++show f)
                         . (resolveWithNameType cx))
                       (D.FieldDescriptorProto.type_name f)
             t (Message {}) = TYPE_MESSAGE
             t (Enumeration {}) = TYPE_ENUM
             t _ = rerr $ unlines [ "Problem found: processFLD cannot resolve type_name to Void or Service"
                                  , "  The parent message is "++maybe "<no message>" toString mp
                                  , "  The field name is "++maybe "<no field name>" toString (D.FieldDescriptorProto.name f)]
             new_type' = (D.FieldDescriptorProto.type' f) `mplus` (fmap (t.snd) r2)
             checkEnumDefault = case (D.FieldDescriptorProto.default_value f,fmap snd r2) of
                                  (Just name,Just (Enumeration values)) | name  `elem` values -> mangleEnum (Just name)
                                                                        | otherwise ->
                                      rerr $ unlines ["Problem found: default enumeration value not recognized:"
                                                     ,"  The parent message is "++maybe "<no message>" toString mp
                                                     ,"  field name is "++maybe "" toString (D.FieldDescriptorProto.name f)
                                                     ,"  bad enum name is "++show (toString name)
                                                     ,"  possible enum values are "++show (map toString values)]
                                  (Just def,_) | new_type' == Just TYPE_MESSAGE
                                                 || new_type' == Just TYPE_GROUP ->
                                    rerr $ "Problem found: You set a default value for a MESSAGE or GROUP: "++unlines [show def,show f]
                                  (maybeDef,_) -> maybeDef
  
             groupName = case mp of
                           Nothing -> resolve cx (D.FieldDescriptorProto.name f)
                           Just p -> do n <- D.FieldDescriptorProto.name f
                                        return (Utf8 . U.fromString . (toString p++) . ('.':) . mangleModuleName . toString $ n)

      processENM cx e = e { D.EnumDescriptorProto.name = resolve cx (D.EnumDescriptorProto.name e)
                          , D.EnumDescriptorProto.value = mangleEnums (D.EnumDescriptorProto.value e) }
      processSRV cx s = s { D.ServiceDescriptorProto.name   = resolve cx (D.ServiceDescriptorProto.name s)
                          , D.ServiceDescriptorProto.method = fmap (processMTD cx) (D.ServiceDescriptorProto.method s) }
      processMTD cx m = m { D.MethodDescriptorProto.name        = mangleFieldName (D.MethodDescriptorProto.name m)
                          , D.MethodDescriptorProto.input_type  = resolve cx (D.MethodDescriptorProto.input_type m)
                          , D.MethodDescriptorProto.output_type = resolve cx (D.MethodDescriptorProto.output_type m) }
  in processFDP protoIn
