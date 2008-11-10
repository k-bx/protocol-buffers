module Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto,makeNameMap,makeNameMaps,Env(..),TopLevel(..),ReMap,NameMap(..)) where

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
import qualified Text.DescriptorProtos.FileDescriptorSet              as D(FileDescriptorSet(FileDescriptorSet))
import qualified Text.DescriptorProtos.FileDescriptorSet              as D.FileDescriptorSet(FileDescriptorSet(..))
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D(MethodDescriptorProto)
import qualified Text.DescriptorProtos.MethodDescriptorProto          as D.MethodDescriptorProto(MethodDescriptorProto(..))
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D(ServiceDescriptorProto)
import qualified Text.DescriptorProtos.ServiceDescriptorProto         as D.ServiceDescriptorProto(ServiceDescriptorProto(..))
import qualified Text.DescriptorProtos.UninterpretedOption            as D(UninterpretedOption)
import qualified Text.DescriptorProtos.UninterpretedOption            as D.UninterpretedOption(UninterpretedOption(..))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D(NamePart(NamePart))
import qualified Text.DescriptorProtos.UninterpretedOption.NamePart   as D.NamePart(NamePart(..))
import qualified Text.DescriptorProtos.EnumOptions      as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions      as D.EnumOptions(EnumOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.EnumValueOptions as D(EnumValueOptions)
import qualified Text.DescriptorProtos.EnumValueOptions as D.EnumValueOptions(EnumValueOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.FieldOptions     as D(FieldOptions)
import qualified Text.DescriptorProtos.FieldOptions     as D.FieldOptions(FieldOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.FileOptions      as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions      as D.FileOptions(FileOptions(..))
import qualified Text.DescriptorProtos.MessageOptions   as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions   as D.MessageOptions(MessageOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.MethodOptions    as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions    as D.MethodOptions(MethodOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.ServiceOptions   as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions   as D.ServiceOptions(ServiceOptions(uninterpreted_option))

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.ProtoCompile.Instances
import Text.ProtocolBuffers.ProtoCompile.Parser

import Control.Applicative
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Writer
import Data.Char
import Data.Ratio
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

--import Debug.Trace(trace)

-- Used by err and throw
indent :: String -> String
indent = unlines . map (\str -> ' ':' ':str) . lines

ishow :: Show a => a -> String
ishow = indent . show

errMsg :: String -> String
errMsg s = "Text.ProtocolBuffers.Resolve fatal error encountered, message:\n"++indent s

err :: forall b. String -> b
err = error . errMsg 

throw :: (Error e, MonadError e m) =>  String -> m a
throw s = throwError (strMsg (errMsg s))

-- | 'annErr' allows 
-- annErr :: (Show e, Error e,MonadError e m) => String -> m a -> m a
-- annErr s act = catchError act (\e -> throwError (strMsg ("annErr: "++s++'\n':ishow e)))

annErr :: (MonadError String m) => String -> m a -> m a
annErr s act = catchError act (\e -> throwError ("annErr: "++s++'\n':indent e))

getJust :: (Error e,MonadError e m, Typeable a) => String -> Maybe a -> m a
{-#  INLINE getJust #-}
getJust s ma@Nothing = throw $ "Impossible? Expected Just of type "++show (typeOf ma)++" but got nothing:\n"++indent s
getJust _s (Just a) = return a

-- insert '.' between each item and convert to utf8 bytes
encodeModuleNames :: [String] -> Utf8
encodeModuleNames [] = Utf8 mempty
encodeModuleNames xs = Utf8 . U.fromString . foldr1 (\a b -> a ++ '.':b) $ xs

splitDot :: Utf8 -> [String]
splitDot = splitDot' . toString

splitDot' :: String -> [String]
splitDot' = unfoldr s where
  s ('.':xs) = s xs
  s [] = Nothing
  s xs = Just (span ('.'/=) xs)
{-
-- By construction in the Lexer.x file, the use of parseIdent here cannot go wrong.
-- But I will be paranoid and write a total function anyway.
-- Call to 'parts' are with a non-empty string that should not start with a '.'
parseIdent :: (Error e,MonadError e m) => Utf8 -> m (Bool,[String])
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
-}
-- This adds a leading dot if the input is non-empty
joinDot :: [IName String] -> FIName String
joinDot [] = err $ "joinDot on an empty list of IName!"
joinDot (x:xs) = fqAppend (promoteFI x) xs

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
          (getFieldId $ maybe minBound FieldId start, getFieldId $ maybe maxBound (FieldId . pred) end)

getExtRanges :: D.DescriptorProto -> [(FieldId,FieldId)]
getExtRanges d = concatMap check unchecked
  where check x@(lo,hi) | hi < lo = []
                        | hi<19000 || 19999<lo  = [x]
                        | otherwise = concatMap check [(lo,18999),(20000,hi)]
        unchecked = F.foldr ((:) . extToPair) [] (D.DescriptorProto.extension_range d)
        extToPair (D.ExtensionRange
                    { D.ExtensionRange.start = start
                    , D.ExtensionRange.end = end }) =
          (maybe minBound FieldId start, maybe maxBound (FieldId . pred) end)

-- | By construction Env is 0 or more Local Entity namespaces followed
-- by 1 or more Global TopLevel namespaces (self and imported files).
-- Entities in first Global TopLevel namespace can refer to each other
-- and to Entities in the list of directly imported TopLevel namespaces only.
data Env = Local Entity {- E'Message -} Env | Global TopLevel [TopLevel]  deriving Show
-- | TopLevel corresponds to all items defined in a .proto file. This
-- includes the FileOptions since this will be consulted when
-- generating the Haskel module names, and the imported files are only
-- known through their TopLevel data.
data TopLevel = TopLevel { top'Path :: FilePath
                         , top'Package :: [IName String]
                         , top'FDP :: Either ErrStr D.FileDescriptorProto -- resolvedFDP'd
                         , top'mVals :: EMap } deriving Show
-- | The EMap type is a local namespace attached to an entity
type EMap = Map (IName String) Entity
-- | An Entity is some concrete item in the namespace of a proto file.
-- All Entity values have a leading-dot fully-qualified with the package "eName".
-- The E'Message,Group,Service have EMap namespaces to inner Entity items.
data Entity = E'Message { eName :: [IName String], validExtensions :: [(FieldId,FieldId)]
                                                 , mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }
            | E'Group   { eName :: [IName String], mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }
            | E'Field   { eName :: [IName String], fNumber :: FieldId, fType :: Maybe D.Type
                                                 , mVal :: Maybe (Either ErrStr Entity) {- E'Message,Group,Enum -} }
            | E'Key     { eName :: [IName String], eMsg :: Either ErrStr Entity         {- E'Message -}
                                                 , fNumber :: FieldId, fType :: Maybe D.Type
                                                 , mVal :: Maybe (Either ErrStr Entity) {- E'Message,Group,Enum -} }
            | E'Enum    { eName :: [IName String], eVals :: Map (IName Utf8) Int32 }
            | E'Service { eName :: [IName String], mVals :: EMap {- E'Method -} }
            | E'Method  { eName :: [IName String], eMsgIn,eMsgOut :: Maybe (Either ErrStr Entity) {- E'Message -} }
            | E'Error String [Entity]
  deriving (Show)

allowed :: Env -> [([IName String],[IName String])]
allowed (Local entity env) = allowedE entity : allowed env
allowed (Global t ts) = map allowedT (t:ts)
allowedE :: Entity -> ([IName String], [IName String])
allowedE entity = ((,)  (eName entity)) $
  case get'mVals entity of
    Nothing -> []
    Just m -> M.keys m
allowedT :: TopLevel -> ([IName String], [IName String])
allowedT tl = (top'Package tl,M.keys (top'mVals tl))

data NameMap = NameMap (FIName Utf8,[MName String],[MName String]) ReMap

-- Create a mapping from the "official" name to the Haskell hierarchy mangled name
type ReMap = Map (FIName Utf8) ProtoName

type RE a = ReaderT Env (Either ErrStr) a

data SEnv = SEnv { my'Parent :: [IName String]
                 , my'Env :: Env }
--                 , my'Template :: ProtoName }

instance Show SEnv where
  show (SEnv p e) = "(SEnv "++show p++" ; "++ whereEnv e ++ ")" --" ; "++show (haskellPrefix t,parentModule t)++ " )"

type ErrStr = String

type SE a = ReaderT SEnv (Either ErrStr) a

fqName :: Entity -> FIName Utf8
fqName = fiFromString . joinDot . eName

fiFromString :: FIName String -> FIName Utf8
fiFromString = FIName . fromString . fiName

diToString :: DIName Utf8 -> DIName String
diToString = DIName . toString . diName

iToString :: IName Utf8 -> IName String
iToString = IName . toString . iName

-- Three entities provide child namespaces: E'Message, E'Group, and E'Service
get'mVals :: Entity -> Maybe EMap
get'mVals (E'Message {mVals = x}) = Just x
get'mVals (E'Group   {mVals = x}) = Just x
get'mVals (E'Service {mVals = x}) = Just x
get'mVals _ = Nothing

-- | This is a helper for resolveEnv
toGlobal :: Env -> Env
toGlobal (Local _entity env) = toGlobal env
toGlobal x@(Global {}) = x

getTL :: Env -> TopLevel
getTL (Local _entity env) = getTL env
getTL (Global tl _tls) = tl

getTLS :: Env -> (TopLevel,[TopLevel])
getTLS (Local _entity env) = getTLS env
getTLS (Global tl tls) = (tl, tls)

-- | This is used for resolving some UninterpretedOption names
resolveHere :: Entity -> Utf8 -> RE Entity
resolveHere parent nameU = do
  let rFail msg = throw ("Could not lookup "++show (toString nameU)++"\n"++indent msg)
  x <- getJust ("resolveHere: validI nameU failed for "++show nameU) (fmap iToString (validI nameU))
  case get'mVals parent of
    Just vals -> case M.lookup x vals of
                   Just entity -> return entity
                   Nothing -> rFail ("because there is no such name here:  "++show (eName parent))
    Nothing -> rFail ("because environment has no local names:\n"++ishow (eName parent))

-- | 'resolveEnv' is the query operation for the Env namespace.  It
-- recorgnizes names beginning with a '.' as already being
-- fully-qualified names. This is called from the different monads via
-- resolveRE, resolveSE, or getType.
resolveEnv :: Utf8 -> Env -> Either ErrStr Entity
resolveEnv nameU envIn = do
  (isGlobal,xs) <- checkDIUtf8 nameU
  let mResult = if isGlobal then lookupEnv (map iToString xs) (toGlobal envIn)
                            else lookupEnv (map iToString xs) envIn
  case mResult of
    Nothing -> throw . unlines $ [ "resolveEnv: Could not lookup "++show nameU
                                 , "which parses as "++show (isGlobal,xs)
                                 , "in environment: "++(whereEnv envIn)
                                 , "allowed: "++show (allowed envIn)]
    Just e@(E'Error {}) -> throw (show e)
    Just e -> return e

resolveRE :: Utf8 -> RE Entity
resolveRE nameU = lift . (resolveEnv nameU) =<< ask

resolveSE :: Utf8 -> SE (Either ErrStr Entity)
resolveSE nameU = fmap (resolveEnv nameU) (asks my'Env)

-- | 'getType' is used to lookup the type strings in service method records.
getType :: Show a => String -> (a -> Maybe Utf8) -> a -> SE (Maybe (Either ErrStr Entity))
getType s f a = do
  typeU <- getJust s (f a)
  case parseType (toString typeU) of
    Just _ -> return Nothing
    Nothing -> do ee <- resolveSE typeU
                  return (Just (expectMGE ee))

-- | 'expectMGE' is used by getType and 'entityField'
expectMGE :: Either ErrStr Entity -> Either ErrStr Entity
expectMGE ee@(Left {}) = ee
expectMGE ee@(Right e) = if isMGE e then ee
                           else Left $ "expectMGE: Name resolution failed to find a Message, Group, or Enum:\n"++ishow e
  where isMGE e' = case e' of E'Message {} -> True
                              E'Group {} -> True
                              E'Enum {} -> True
                              _ -> False

-- | 'expectM' is used by 'entityField'
expectM :: Either ErrStr Entity -> Either ErrStr Entity
expectM ee@(Left {}) = ee
expectM ee@(Right e) = if isMGE e then ee
                         else Left $ "expectMGE: Name resolution failed to find a Message, Group, or Enum:\n"++ishow (eName e)
  where isMGE e' = case e' of E'Message {} -> True
                              _ -> False

expectFK :: Entity -> RE Entity
expectFK e = if isFK e then return e
               else throwError $ "expectF: Name resolution failed to find a Field or Key:\n"++ishow (eName e)
  where isFK e' = case e' of E'Field {} -> True
                             E'Key {} -> True
                             _ -> False

-- | This is a helper for resolveEnv for error messages
whereEnv :: Env -> String
whereEnv (Local entity env) = fiName (joinDot (eName entity)) ++ " in "++show (top'Path . getTL $ env)
whereEnv (Global tl _) = fiName (joinDot (top'Package tl)) ++ " in " ++ show (top'Path tl)

-- | lookupEnv is used only by resolveEnv
lookupEnv :: [IName String] -> Env -> Maybe Entity
lookupEnv xs (Global tl tls) = lookupTopLevel xs tl <|> msum (map (lookupTopLevel xs) tls)
lookupEnv _xs (Local e@(E'Error {}) _env) = return e
lookupEnv xs (Local entity env) = case get'mVals entity of
                                    Just vals -> lookupVals vals xs <|> lookupEnv xs env
                                    Nothing -> Nothing

-- | lookupTopLeve is used only by lookupEnv
lookupTopLevel :: [IName String] -> TopLevel -> Maybe Entity
lookupTopLevel xs tl =
  lookupVals (top'mVals tl) xs <|> (stripPrefix (top'Package tl) xs >>= lookupVals (top'mVals tl))

-- | lookupVals is used by lookupEnv and lookupTopLevel
lookupVals :: EMap -> [IName String] -> Maybe Entity
lookupVals _vals [] = Nothing
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
unique :: IName String -> Entity -> Entity -> Entity
unique name (E'Error _ a) (E'Error _ b) = E'Error ("Namespace collision for "++show name) (a++b)
unique name (E'Error _ a) b = E'Error ("Namespace collision for "++show name) (a++[b])
unique name a (E'Error _ b) = E'Error ("Namespace collision for "++show name) (a:b)
unique name a b = E'Error ("Namespace collision for "++show name) [a,b]

maybeM :: Monad m => (x -> m a) -> (Maybe x) -> m (Maybe a)
maybeM f mx = maybe (return Nothing) (liftM Just . f) mx

runSE :: SEnv -> SE a -> Either ErrStr a
runSE sEnv m = runReaderT m sEnv

makeNameMaps :: [MName String] -> Env -> Either ErrStr NameMap
makeNameMaps hPrefix env = do
  let (tl,tls) = getTLS env
  (fdp:fdps) <- mapM top'FDP (tl:tls)
  (NameMap tuple m) <- makeNameMap hPrefix fdp
  let f (NameMap _ x) = x
  ms <- fmap (map f) . mapM (makeNameMap hPrefix) $ fdps
  return (NameMap tuple (M.unions (m:ms)))

-- | 'makeNameMap' conservatively checks its input.
makeNameMap :: [MName String] -> D.FileDescriptorProto -> Either ErrStr NameMap
makeNameMap hPrefix fdpIn = go (makeOne fdpIn) where
  go = fmap ((\(a,w) -> NameMap a (M.fromList w))) . runWriterT
--  makeOne :: D.FileDescriptorProto -> WriterT [(FIName Utf8,ProtoName)] (Either ErrStr) ()
  makeOne fdp = do
    -- Create 'template' :: ProtoName using "Text.ProtocolBuffers.Identifiers"
    rawPackage <- getJust "makeNameMap.makeOne: D.FileDescriptorProto.package"
                  (D.FileDescriptorProto.package fdp)
    lift (checkDIUtf8 rawPackage) -- guard-like effect
    let packageName = difi (DIName rawPackage)
    rawParent <- getJust "makeNameMap.makeOne: " . msum $
        [ D.FileOptions.java_outer_classname =<< (D.FileDescriptorProto.options fdp)
        , D.FileOptions.java_package =<< (D.FileDescriptorProto.options fdp)
        , D.FileDescriptorProto.package fdp]
    diParent <- getJust ("makeNameMap.makeOne: invalid character in: "++show rawParent)
                  (validDI rawParent)
    let hParent = map (mangle :: IName Utf8 -> MName String) . splitDI $ diParent
        template = ProtoName packageName hPrefix hParent
                     (error "makeNameMap.makeOne.template.baseName undefined")
    list <- runReaderT (mrmFile fdp) template
    return (packageName,hPrefix,hParent)
  -- Traversal of the named DescriptorProto types
  mrmFile :: D.FileDescriptorProto -> MRM ()
  mrmFile fdp = do
    F.mapM_ mrmMsg     (D.FileDescriptorProto.message_type fdp)
    F.mapM_ mrmField   (D.FileDescriptorProto.extension    fdp)
    F.mapM_ mrmEnum    (D.FileDescriptorProto.enum_type    fdp)
    F.mapM_ mrmService (D.FileDescriptorProto.service      fdp)
  mrmMsg dp = do
    template <- mrmName "mrmMsg.name" D.DescriptorProto.name dp
    local (const template) $ do
      F.mapM_ mrmEnum    (D.DescriptorProto.enum_type   dp)
      F.mapM_ mrmField   (D.DescriptorProto.extension   dp)
      F.mapM_ mrmField   (D.DescriptorProto.field       dp)
      F.mapM_ mrmMsg     (D.DescriptorProto.nested_type dp)
  mrmField fdp = mrmName "mrmField.name" D.FieldDescriptorProto.name fdp
  mrmEnum edp = do
    template <- mrmName "mrmEnum.name" D.EnumDescriptorProto.name edp
    local (const template) $ F.mapM_ mrmEnumValue (D.EnumDescriptorProto.value edp)
  mrmEnumValue evdp = mrmName "mrmEnumValue.name" D.EnumValueDescriptorProto.name evdp
  mrmService sdp = do
    template <- mrmName "mrmService.name" D.ServiceDescriptorProto.name sdp
    local (const template) $ F.mapM_ mrmMethod (D.ServiceDescriptorProto.method sdp)
  mrmMethod mdp = mrmName "mrmMethod.name" D.MethodDescriptorProto.name mdp

type MRM a = ReaderT ProtoName (WriterT [(FIName Utf8,ProtoName)] (Either ErrStr)) a

mrmName :: String -> (a -> Maybe Utf8) -> a -> MRM ProtoName
mrmName s f a = do
  template <- ask
  iSelf <- getJust s (validI =<< f a)
  let mSelf = mangle iSelf
      fqSelf = fqAppend (protobufName template) [iSelf]
      self = template { protobufName = fqSelf
                      , baseName = mSelf }
      template' = template { protobufName = fqSelf
                           , parentModule = parentModule template ++ [mSelf] }
  tell [(fqSelf,self)]
  return template'

-- XXX really need to clean this up once this module compiles!
getNames :: String -> (a -> Maybe Utf8) -> a -> SE (IName String,[IName String])
getNames errorMessage accessor record = do
  parent <- asks my'Parent
  iSelf <- getJust errorMessage (validI =<< accessor record)
  let names@(n:ns) = parent ++ [ iToString iSelf ]
  let ans = (iToString iSelf,names)
      toshow = (iSelf,names,parent)
  return ans

descend :: [IName String] -> Entity -> SE a -> SE a
descend names entity act = local mutate act
  where mutate (SEnv _parent env) = SEnv parent' env'
          where parent' = names -- cannot call eName ename, will cause <<loop>> with "getNames" -- XXX revisit
                env' = Local entity env

-- Run each element of (Seq x) as (f x) with same initial environment and state.
-- Then merge the output states and sort out the failures and successes.
kids :: (x -> SE (IName String,Entity)) -> Seq x -> SE ([ErrStr],[(IName String,Entity)])
kids f xs = do sEnv <- ask
               let ans = map (runSE sEnv) . map f . F.toList $ xs
               return (partEither ans)

-- | XXX 'makeTopLevel' takes a .proto file's FileDescriptorProto and the
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
--
-- mdo notes: sEnv depends on global which depends on sEnv ...
makeTopLevel :: D.FileDescriptorProto -> [IName String] -> [TopLevel] -> Either ErrStr Env {- Global -}
makeTopLevel fdp packageName imports = mdo
  filePath <- getJust "makeTopLevel.filePath" (D.FileDescriptorProto.name fdp)
  let sEnv = SEnv packageName global
      groupNames = mapMaybe validI . map toString . mapMaybe D.FieldDescriptorProto.type_name
                 . filter (maybe False (TYPE_GROUP ==) . D.FieldDescriptorProto.type') 
                 $ (F.toList . D.FileDescriptorProto.extension $ fdp)
      isGroup = (`elem` groupNames)
  global <- runSE sEnv (do
    (bads,children) <- fmap unzip . sequence $
      [ kids (entityMsg isGroup) (D.FileDescriptorProto.message_type fdp)
      , kids (entityField True)  (D.FileDescriptorProto.extension    fdp)
      , kids entityEnum          (D.FileDescriptorProto.enum_type    fdp)
      , kids entityService       (D.FileDescriptorProto.service      fdp) ]
    let global' = Global (TopLevel (toString filePath)
                                   packageName
                                   (resolveFDP fdp global')
                                   (M.fromListWithKey unique (concat children)))
                         imports
        bad = unlines (concat bads)
    when (not (null bad)) $
      throw $ "makeTopLevel.bad: Some children failed for "++show filePath++"\n"++bad
    return global'
   )
  return global

{- ***

All the entity* functions are used by makeTopLevel and each other.
They are very scrupulous in being total functions, there is no use of
'error' or 'undefined' and all failures (many of which are Impossible)
are reported by hopefully sensible (Left String) messages.

 *** -}

entityMsg :: (IName String -> Bool) -> D.DescriptorProto -> SE (IName String,Entity)
entityMsg isGroup dp = annErr ("entityMsg "++show (D.DescriptorProto.name dp)) $ mdo
  (self,names) <- getNames "entityMsg.name" D.DescriptorProto.name dp
  numbers <- fmap Set.fromList . mapM (getJust "entityMsg.field.number" . D.FieldDescriptorProto.number) . F.toList . D.DescriptorProto.field $ dp
  when (Set.size numbers /= Seq.length (D.DescriptorProto.field dp)) $
    throwError $ "entityMsg.field.number: There must be duplicate field numbers for "++show names++"\n "++show numbers
  let groupNames = mapMaybe validI . map toString . mapMaybe D.FieldDescriptorProto.type_name
                 . filter (maybe False (TYPE_GROUP ==) . D.FieldDescriptorProto.type') 
                 $ (F.toList . D.DescriptorProto.field $ dp) ++ (F.toList . D.DescriptorProto.extension $ dp)
      isGroup' = (`elem` groupNames)
  entity <- descend names entity $ do
    (bads,children) <- fmap unzip . sequence $
      [ kids entityEnum           (D.DescriptorProto.enum_type   dp)
      , kids (entityField True)   (D.DescriptorProto.extension   dp)
      , kids (entityField False)  (D.DescriptorProto.field       dp)
      , kids (entityMsg isGroup') (D.DescriptorProto.nested_type dp) ]
    let entity' | isGroup self = E'Group names (M.fromListWithKey unique (concat children))
                | otherwise = E'Message names (getExtRanges dp) (M.fromListWithKey unique (concat children))
        bad = unlines (concat bads)
    when (not (null bad)) $
      throwError $ "entityMsg.bad: Some children failed for "++show names++"\n"++bad
    return entity'
  return (self,entity)

entityField :: Bool -> D.FieldDescriptorProto -> SE (IName String,Entity)
entityField isKey fdp = annErr ("entityField "++show fdp) $ do
  (self,names) <- getNames "entityField.name" D.FieldDescriptorProto.name fdp
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    throwError $ "entityField: Impossible? Expected key and got field or vice-versa:\n"++ishow ((isKey,isKey'),names,fdp)
  number <- getJust "entityField.name" . D.FieldDescriptorProto.number $ fdp
  let mType = D.FieldDescriptorProto.type' fdp
  typeName <- maybeM (fmap expectMGE . resolveSE) (D.FieldDescriptorProto.type_name fdp)
  if isKey then do extendee <- fmap expectM . resolveSE =<< getJust "entityField.extendee" (D.FieldDescriptorProto.extendee fdp)
                   return (self,E'Key names extendee (FieldId number) mType typeName)
           else return (self,E'Field names (FieldId number) mType typeName)

entityEnum :: D.EnumDescriptorProto -> SE (IName String,Entity)
entityEnum edp@(D.EnumDescriptorProto {D.EnumDescriptorProto.value=vs}) = do
  (self,names) <- getNames "entityEnum.name" D.EnumDescriptorProto.name edp
  values <- mapM (getJust "entityEnum.value.number" . D.EnumValueDescriptorProto.number) . F.toList $ vs
{- Cannot match protoc if I enable this as a fatal check here
  when (Set.size (Set.fromList values) /= Seq.length vs) $
    throwError $ "entityEnum.value.number: There must be duplicate enum values for "++show names++"\n "++show values
-}
  justNames <- mapM (\v -> getJust ("entityEnum.value.name failed for "++show v) (D.EnumValueDescriptorProto.name v))
               . F.toList $ vs
  valNames <- mapM (\n -> getJust ("validI of entityEnum.value.name failed for "++show n) (validI n)) justNames
  let mapping = M.fromList (zip valNames values)
  when (M.size mapping /= Seq.length vs) $
    throwError $ "entityEnum.value.name: There must be duplicate enum names for "++show names++"\n "++show valNames
  let entity = E'Enum names mapping
  descend names entity $ F.mapM_ entityEnumValue vs
  return (self,E'Enum names mapping) -- discard values

entityEnumValue :: D.EnumValueDescriptorProto -> SE ()
entityEnumValue evdp = do -- Merely use getNames to add mangled self to ReMap state
  getNames "entityEnumValue.name" D.EnumValueDescriptorProto.name evdp
  return ()

entityService :: D.ServiceDescriptorProto -> SE (IName String,Entity)
entityService sdp = mdo
  (self,names) <- getNames "entityService.name" D.ServiceDescriptorProto.name sdp
  let entity = E'Service names (M.fromListWithKey unique methods)
  (badMethods,methods) <- descend names entity $
                          kids entityMethod (D.ServiceDescriptorProto.method sdp)
  when (not (null badMethods)) $
    throwError $ "entityService.badMethods: Some methods failed for "++show names++"\n"++unlines badMethods
  return (self,entity)

entityMethod :: D.MethodDescriptorProto -> SE (IName String,Entity)
entityMethod mdp = do
  (self,names) <- getNames "entityMethod.name" D.MethodDescriptorProto.name mdp
  inputType <- getType "entityMethod.input_type" D.MethodDescriptorProto.input_type mdp
  outputType <- getType "entityMethod.output_type" D.MethodDescriptorProto.output_type mdp
  return (self,E'Method names inputType outputType)

{- ***

The namespace Env is used to transform the original
FileDescriptorProto into a canonical FileDescriptorProto. The new goal
is to match the transformation done by Google's protoc program.  This
will allow the "front end" vs "back end" of each program to
cross-couple, which will at least allow better testing of hprotoc and
the new UninterpretedOption support.

The UninterpretedOption fields are converted by the resolveFDP code below.

These should be total functions with no 'error' or 'undefined' values
possible.

*** -}

resolveFDP :: D.FileDescriptorProto -> Env -> Either ErrStr D.FileDescriptorProto
resolveFDP = runReaderT . fqFileDP 

fqFail :: Show a => String -> a -> Entity -> RE b
fqFail msg dp entity = do
  env <- ask
  throw $ unlines [ msg, "resolving: "++show dp, "in environment: "++whereEnv env, "found: "++show entity ]

fqFileDP :: D.FileDescriptorProto -> RE D.FileDescriptorProto
fqFileDP fdp = do
  newMessages <- T.mapM fqMessage      (D.FileDescriptorProto.message_type fdp)
  newEnums    <- T.mapM fqEnum         (D.FileDescriptorProto.enum_type    fdp)
  newServices <- T.mapM fqService      (D.FileDescriptorProto.service      fdp)
  newKeys     <- T.mapM (fqField True) (D.FileDescriptorProto.extension    fdp)
  consumeUNO $ fdp { D.FileDescriptorProto.message_type = newMessages
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
  local (\env -> (Local entity env)) $ do
    newFields   <- T.mapM (fqField False) (D.DescriptorProto.field       dp)
    newKeys     <- T.mapM (fqField True)  (D.DescriptorProto.extension   dp)
    newMessages <- T.mapM fqMessage       (D.DescriptorProto.nested_type dp)
    newEnums    <- T.mapM fqEnum          (D.DescriptorProto.enum_type   dp)
    consumeUNO $ dp { D.DescriptorProto.field       = newFields
                    , D.DescriptorProto.extension   = newKeys
                    , D.DescriptorProto.nested_type = newMessages
                    , D.DescriptorProto.enum_type   = newEnums }

fqService :: D.ServiceDescriptorProto -> RE D.ServiceDescriptorProto
fqService sdp = do
  entity <- resolveRE =<< getJust "fqService.name" (D.ServiceDescriptorProto.name sdp)
  case entity of
    E'Service {} -> do newMethods <- local (Local entity) $ T.mapM fqMethod (D.ServiceDescriptorProto.method sdp)
                       consumeUNO $ sdp { D.ServiceDescriptorProto.method = newMethods }
    _ -> fqFail "fqService.entity: did not resolve to a service:" sdp entity

fqMethod :: D.MethodDescriptorProto -> RE D.MethodDescriptorProto
fqMethod mdp = do
  entity <- resolveRE =<< getJust "fqMethod.name" (D.MethodDescriptorProto.name mdp)
  case entity of
    E'Method {} -> do mdp1 <- case eMsgIn entity of
                                Nothing -> return mdp
                                Just resolveIn -> do new <- fmap fqName (lift resolveIn)
                                                     return (mdp {D.MethodDescriptorProto.input_type = Just (fiName new)})
                      mdp2 <- case eMsgOut entity of
                                Nothing -> return mdp1
                                Just resolveIn -> do new <- fmap fqName (lift resolveIn)
                                                     return (mdp1 {D.MethodDescriptorProto.output_type = Just (fiName new)})
                      consumeUNO mdp2
    _ -> fqFail "fqMethod.entity: did not resolve to a Method:" mdp entity

-- The field is a bit more complicated to resolve.  The Key variant
-- needs to resolve the extendee.  The type code from Parser.hs might
-- be Nothing and this needs to be resolved to TYPE_MESSAGE or
-- TYPE_ENUM, and if it is the latter then any default value string is
-- checked for validity.
fqField :: Bool -> D.FieldDescriptorProto -> RE D.FieldDescriptorProto
fqField isKey fdp = annErr ("fqField "++show fdp) $ do
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    ask >>= \env -> throwError $ "fqField.isKey: Expected key and got field or vice-versa:\n"++ishow ((isKey,isKey'),whereEnv env,fdp)
  entity <- expectFK =<< resolveRE =<< getJust "fqField.name" (D.FieldDescriptorProto.name fdp)
  newExtendee <- case (isKey,entity) of
                   (True,E'Key {}) -> do
                      ext <- lift (eMsg entity)
                      case ext of
                        E'Message {} -> when (not (checkFI (validExtensions ext) (fNumber entity))) $
                          throwError $ "fqField.newExtendee: Field Number of extention key invalid:\n"
                            ++unlines ["Number is "++show (fNumber entity),"Valid ranges: "++show (validExtensions ext),"Extendee: "++show (eName ext),"Descriptor: "++show fdp]
                        _ -> fqFail "fqField.ext: Key's target is not an E'Message:" fdp ext
                      fmap (Just . fiName . fqName) . lift . eMsg $ entity
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
    (Just ee@(E'Enum {eVals = enumVals}),Just enumVal) ->
      let badVal = throwError $ "fqField.default_value: Default enum value is invalid:\n"
                     ++unlines ["Value is "++show (toString enumVal)
                               ,"Allowed values from "++show (eName ee)
                               ," are "++show (M.keys enumVals)
                               ,"Descriptor: "++show fdp]
      in case validI enumVal of
          Nothing -> badVal
          Just iVal -> when (M.notMember iVal enumVals) badVal
    _ -> return ()
  consumeUNO $
    if isKey then (fdp { D.FieldDescriptorProto.extendee  = newExtendee
                       , D.FieldDescriptorProto.type'     = Just actualType
                       , D.FieldDescriptorProto.type_name = fmap (fiName . fqName) mTypeName })
             else (fdp { D.FieldDescriptorProto.type'     = Just actualType
                       , D.FieldDescriptorProto.type_name = fmap (fiName . fqName) mTypeName })

fqEnum :: D.EnumDescriptorProto -> RE D.EnumDescriptorProto
fqEnum edp = do
  entity <- resolveRE =<< getJust "fqEnum.name" (D.EnumDescriptorProto.name edp)
  case entity of
    E'Enum {} -> do evdps <- T.mapM consumeUNO (D.EnumDescriptorProto.value edp)
                    consumeUNO $ edp { D.EnumDescriptorProto.value = evdps }
    _ -> fqFail "fqEnum.entity: did not resolve to an E'Enum:" edp entity

{- The consumeUNO calls above hide this cut-and-pasted boilerplate -}

class ConsumeUNO a where consumeUNO :: a -> RE a

instance ConsumeUNO D.EnumDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.EnumDescriptorProto.options = Just o })
                       (D.EnumDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "EnumOptions" m (D.EnumOptions.uninterpreted_option m)
                            return (m' { D.EnumOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.EnumValueDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.EnumValueDescriptorProto.options = Just o })
                       (D.EnumValueDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "EnumValueOptions" m (D.EnumValueOptions.uninterpreted_option m)
                            return (m' { D.EnumValueOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.MethodDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.MethodDescriptorProto.options = Just o })
                       (D.MethodDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "MethodOptions" m (D.MethodOptions.uninterpreted_option m)
                            return (m' { D.MethodOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.ServiceDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.ServiceDescriptorProto.options = Just o })
                       (D.ServiceDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "ServiceOptions" m (D.ServiceOptions.uninterpreted_option m)
                            return (m' { D.ServiceOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.FieldDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.FieldDescriptorProto.options = Just o })
                       (D.FieldDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "FieldOptions" m (D.FieldOptions.uninterpreted_option m)
                            return (m' { D.FieldOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.FileDescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.FileDescriptorProto.options = Just o })
                       (D.FileDescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "FileOptions" m (D.FileOptions.uninterpreted_option m)
                            return (m' { D.FileOptions.uninterpreted_option = mempty })

instance ConsumeUNO D.DescriptorProto where
  consumeUNO a = maybe (return a) (processOpt >=> \o -> return $ a { D.DescriptorProto.options = Just o })
                       (D.DescriptorProto.options a)
    where processOpt m = do m' <- interpretOptions "MessageOptions" m (D.MessageOptions.uninterpreted_option m)
                            return (m' { D.MessageOptions.uninterpreted_option = mempty })

{- The boilerplate above feeds interpretOptions to do the real work -}

-- 'interpretOptions' is used by the 'consumeUNO' instances
-- This prepends the ["google","protobuf"] and updates all the options into the ExtField of msg
interpretOptions :: ExtendMessage msg => String -> msg -> Seq D.UninterpretedOption -> RE msg
interpretOptions name msg unos = do
  name' <- getJust ("interpretOptions: invalid name "++show name) (validI name)
  ios <- mapM (interpretOption [IName "google",IName "protobuf",name']) . F.toList $ unos
  let (ExtField ef) = getExtField msg
      ef' = foldl' (\m (k,v) -> seq v $ M.insert k v m) ef ios
      msg' = seq ef' (putExtField (ExtField ef') msg)
  return msg'

-- 'interpretOption' is called by 'interpretOptions'
-- The 'interpretOption' function is quite long because there are two things going on.
-- The first is the actual type must be retrieved from the UninterpretedOption and encoded.
-- The second is that messages/groups holding messages/groups ... holding the above must wrap this.
-- Both of the above might come from extension keys or from field names.
-- And as usual, there are many ways thing could conceivable go wrong or be out of bounds.
--
-- The first parameter must be a name such as ["google","protobuf","FieldOption"]
interpretOption :: [IName String] -> D.UninterpretedOption -> RE (FieldId,ExtFieldValue)
interpretOption optName uno = case F.toList (D.UninterpretedOption.name uno) of
                                [] -> iFail $ "Empty name_part"
                                (part:parts) -> go Nothing optName part parts
 where
  iFail msg = do env <- ask
                 throw $ unlines [ "interpretOption: Failed to handle UninterpretedOption for: "++show optName
                                 , "  environment: "++whereEnv env
                                 , "  value: "++show uno
                                 , "  message: "++msg ]

  -- This takes care of an intermediate message or group type
  go mParent names (D.NamePart { D.NamePart.name_part = name
                               , D.NamePart.is_extension = isKey }) (next:rest) = do
    -- get entity (Field or Key) and the TYPE_*
    (fk,entity) <-
      if not isKey
        then case mParent of
               Nothing -> iFail $ "Cannot resolve local (is_extension False) name, no parent; expected (key)."
               Just parent -> do
                 entity'field <- resolveHere parent name
                 case entity'field of
                   E'Field {} -> case mVal entity'field of
                                   Nothing -> iFail $ "Intermediate entry E'Field is of basic type, not E'Message or E'Group: "++show (names,eName entity'field)
                                   Just val -> lift val >>= \e -> return (entity'field,e)
                   _ -> iFail $ "Name "++show (toString name)++" was resolved but was not an E'Field: "++show (eName entity'field)
        else do entity'key <- resolveRE name
                case entity'key of
                  E'Key {} -> do extendee <- lift (eMsg entity'key)
                                 when (eName extendee /= names) $
                                   iFail $ "Intermediate entry E'Key extends wrong type: "++show (names,eName extendee)
                                 case mVal entity'key of
                                   Nothing-> iFail $ "Intermediate entry E'Key is of basic type, not E'Message or E'Group: "++show (names,eName entity'key)
                                   Just val -> lift val >>= \e -> return (entity'key,e)
                  _ -> iFail $ "Name "++show (toString name)++" was resolved but was not an E'Key: "++show (eName entity'key)
    t <- case entity of
      E'Message {} -> return TYPE_MESSAGE
      E'Group {} -> return TYPE_GROUP
      _ -> iFail $ "Intermediate entry is not an E'Message or E'Group: "++show (eName entity)
    -- recursive call to get inner result
    (fid',ExtFromWire wt' raw') <- go (Just entity) (eName entity) next rest
    -- wrap old tag + inner result with outer info
    let tag' = getWireTag (mkWireTag fid' wt')
        bs' = Seq.index raw' 0
    let fid = fNumber fk
        wt = toWireType (FieldType (fromEnum t))
        raw = Seq.singleton . runPut $
          case t of TYPE_MESSAGE -> do putSize (size'Varint tag' + LC.length bs')
                                       putVarUInt tag'
                                       putLazyByteString bs'
                    TYPE_GROUP -> do putVarUInt tag'
                                     putLazyByteString bs'
                                     putVarUInt (succ (getWireTag (mkWireTag fid wt)))
                    _ -> fail $ "bug! raw with type "++show t++" should be impossible"
    return (fid,ExtFromWire wt raw)

  -- This takes care of the acutal value of the option, which must be a basic type
  go mParent names (D.NamePart { D.NamePart.name_part = name
                                , D.NamePart.is_extension = isKey }) [] = do
    -- get entity (Field or Key) and the TYPE_*
    fk <- if isKey then resolveRE name
                else case mParent of
                       Just parent -> resolveHere parent name
                       Nothing -> iFail $ "Cannot resolve local (is_extension False) name, no parent; expected (key)."
    case fk of
      E'Field {} | not isKey -> return ()
      E'Key {} | isKey -> do
        ext <- lift (eMsg fk)
        when (eName ext /= names) $ iFail $ "Last entry E'Key extends wrong type: "++show (names,eName ext)
      _ -> iFail $ "Last entity was resolved but was not an E'Field or E'Key: "++show fk
    t <- case (fType fk) of
           Nothing -> return TYPE_ENUM
           Just TYPE_GROUP -> iFail $ "Last entry was a TYPE_GROUP instead of concrete value type"
           Just TYPE_MESSAGE -> {- impossible -} iFail $ "Last entry was a TYPE_MESSAGE instead of concrete value type"
           Just typeCode -> return typeCode
    -- Need to define a polymorphic 'done' to convert actual data type to its wire encoding
    let done :: Wire v => v -> RE (FieldId,ExtFieldValue)
        done v = let ft = FieldType (fromEnum t)
                     wt = toWireType ft
                     fid = fNumber fk
                 in return (fid,ExtFromWire wt (Seq.singleton (runPut (wirePut ft v))))
    -- The actual type and value fed to 'done' depends on the values 't' and 'uno':
    case t of
      TYPE_ENUM ->
        case (mVal fk,D.UninterpretedOption.identifier_value uno) of
          (Just (Right (E'Enum {eVals=enumVals})),Just enumVal) ->
            case validI enumVal of
              Nothing -> iFail $ "invalid D.UninterpretedOption.identifier_value: "++show enumVal
              Just iVal -> case M.lookup iVal enumVals of
                             Nothing -> iFail $ "enumVal lookup failed: "++show (iVal,M.keys enumVals)
                             Just val -> done (fromEnum val) -- fromEnum :: Int32 -> Int
          (Just (Right (E'Enum {})),Nothing) -> iFail $ "No identifer_value value to lookup in E'Enum"
          (me,_) -> iFail $ "Expected Just E'Enum, got: "++show me
      TYPE_STRING   -> do
        bs <- getJust "UninterpretedOption.string_value" (D.UninterpretedOption.string_value uno)
        maybe (done (Utf8 bs)) (\i -> iFail $ "Invalid utf8 in string_value at index: "++show i)
              (isValidUTF8 bs)
      TYPE_BYTES    -> done =<< getJust "UninterpretedOption.string_value" (D.UninterpretedOption.string_value uno)
      TYPE_BOOL     -> done =<< bVal
      TYPE_DOUBLE   -> done =<< dVal
      TYPE_FLOAT    -> done =<< asFloat =<< dVal
      TYPE_INT64    -> done =<< (iVal :: RE Int64)
      TYPE_SFIXED64 -> done =<< (iVal :: RE Int64)
      TYPE_SINT64   -> done =<< (iVal :: RE Int64)
      TYPE_UINT64   -> done =<< (iVal :: RE Word64)
      TYPE_FIXED64  -> done =<< (iVal :: RE Word64)
      TYPE_INT32    -> done =<< (iVal :: RE Int32)
      TYPE_SFIXED32 -> done =<< (iVal :: RE Int32)
      TYPE_SINT32   -> done =<< (iVal :: RE Int32)
      TYPE_UINT32   -> done =<< (iVal :: RE Word32)
      TYPE_FIXED32  -> done =<< (iVal :: RE Word32)
      _ -> iFail $ "bug! go with type "++show t++" should be impossible"

  -- Machinery needed by the final call of go
  bVal :: RE Bool
  bVal = let true = Utf8 (U.fromString "true"); false = Utf8 (U.fromString "false")
         in case D.UninterpretedOption.identifier_value uno of
              Just s | s == true -> return True
                     | s == false -> return False
              _ -> iFail "Expected 'true' or 'false' identifier_value"
  dVal :: RE Double
  dVal = case (D.UninterpretedOption.negative_int_value uno
              ,D.UninterpretedOption.positive_int_value uno
              ,D.UninterpretedOption.double_value uno) of
           (_,_,Just d) -> return d
           (_,Just p,_) -> return (fromIntegral p)
           (Just n,_,_) -> return (fromIntegral n)
           _ -> iFail "No numeric value"
  asFloat :: Double -> RE Float
  asFloat d = let fmax :: Ratio Integer
                  fmax = (2-(1%2)^(23::Int)) * (2^(127::Int))
                  d' = toRational d
              in if (negate fmax <= d') && (d' <= fmax)
                   then return (fromRational d')
                   else iFail $ "Double out of range for Float: "++show d
  rangeCheck :: forall a. (Bounded a,Integral a) => Integer -> RE a
  rangeCheck i = let r = (toInteger (minBound ::a),toInteger (maxBound :: a))
                 in if inRange r i then return (fromInteger i) else iFail $ "Constant out of range: "++show (r,i)
  asInt :: Double -> RE Integer
  asInt x = let (a,b) = properFraction x
            in if b==0 then return a
                 else iFail $ "Double value not an integer: "++show x
  iVal :: (Bounded y, Integral y) => RE y
  iVal = case (D.UninterpretedOption.negative_int_value uno
              ,D.UninterpretedOption.positive_int_value uno
              ,D.UninterpretedOption.double_value uno) of
           (_,Just p,_) -> rangeCheck (toInteger p)
           (Just n,_,_) -> rangeCheck (toInteger n)
           (_,_,Just d) -> rangeCheck =<< asInt d
           _ -> iFail "No numeric value"

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile paths target = do
  let test [] = return Nothing
      test (path:rest) = do
        let fullname = combine path target
        found <- doesFileExist fullname
        if found then return (Just fullname)
          else test rest
  test paths

type Stuff = (Env,D.FileDescriptorProto) -- XXX will change

loadProto :: [FilePath] -> FilePath -> IO Stuff
loadProto protoDirs protoFile = goState (load Set.empty protoFile) where
  goState act = evalStateT act mempty
  loadFailed f msg = fail . unlines $ ["Parsing proto:",f,"has failed with message",msg]
  load :: Set.Set FilePath -> FilePath -> StateT (Map FilePath Stuff) IO Stuff
  load parentsIn file = do
    built <- get
    when (Set.member file parentsIn)
         (loadFailed file (unlines ["imports failed: recursive loop detected"
                                   ,unlines . map show . M.assocs $ built,show parentsIn]))
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
            parsed'fdp <- either (loadFailed toRead . show) return $
                          (parseProto toRead proto)
            packageName <- either (loadFailed toRead . show) (return . map iToString . snd) $
                           (checkDIUtf8 =<< getJust "makeTopLevel.packageName" (D.FileDescriptorProto.package parsed'fdp))
            let parents = Set.insert file parentsIn
                importList = map toString . F.toList . D.FileDescriptorProto.dependency $ parsed'fdp
            imports <- mapM (fmap (getTL . fst) . load parents) importList
            let eEnv = makeTopLevel parsed'fdp packageName imports
            global'env <- either (loadFailed file) return eEnv
            resolved'fdp <- either (loadFailed file) return (top'FDP . getTL $ global'env)
            liftIO $ dump resolved'fdp
            let result = (global'env, resolved'fdp)
            modify (M.insert file result) -- add to memorized results
            return result

dump :: D.FileDescriptorProto -> IO ()
dump fdp = do
  putStrLn $ "dumping: "++show (D.FileDescriptorProto.name fdp)
  let f = "/tmp/dumpfdp"
  LC.writeFile f (messagePut $ defaultValue { D.FileDescriptorSet.file = Seq.singleton fdp })
  print (show f++" has been dumped")

{-
-- up-case the first letter after each '.', Nothing -> Nothing
-- mangleCaps :: Maybe Utf8 -> String
-- mangleCaps Nothing = ""
-- mangleCaps x = foldr1 (\a b -> a ++ '.':b) (mangleCap x)

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
mangleFieldName' :: String -> String
mangleFieldName' = fixname
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

-- down-case the first letter, add single quote if it is a reserved word
mangleFieldName :: Maybe Utf8 -> Maybe Utf8
mangleFieldName = fmap (Utf8 . U.fromString . mangleFieldName' . toString)


-- up-case the first letter after each '.', Nothing -> ""
mangleCap :: Maybe Utf8 -> [String]
mangleCap = mangleModuleNames . fromMaybe (Utf8 mempty)
  where mangleModuleNames :: Utf8 -> [String]
        mangleModuleNames bs = map mangleModuleName . splitDot $ bs

-}

{-
newtype NameSpace = NameSpace {unNameSpace::(Map String ([String],NameType,Maybe NameSpace))}
  deriving (Show,Read)
data NameType = Message [(Int32,Int32)] | Enumeration [Utf8] | Service | Void 
  deriving (Show,Read)
type Context = [NameSpace]

seeContext :: Context -> [String] 
seeContext cx = map ((++"[]") . concatMap (\k -> show k ++ ", ") . M.keys . unNameSpace) cx

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
--    prefix = splitDot (just "toContext.prefix" (D.FileDescriptorProto.package protoIn))
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
            rMsg = "*** Name resolution failed:\n"
                   ++unlines ["Unmangled name: "++show bsIn
                             ,"Mangled name: "++show nameIn
                             ,"List of known names:"]
                   ++ unlines (seeContext context)
            resolver [] (NameSpace _cx) = rerr $ "Impossible? case in Text.ProtocolBuffers.Resolve.resolveWithNameType.resolver []\n" ++ rMsg
            resolver [name] (NameSpace cx) = case M.lookup name cx of
                                               Nothing -> Nothing
                                               Just (fullName,nameType,_) -> Just (encodeModuleNames fullName,nameType)
            resolver (name:rest) (NameSpace cx) = case M.lookup name cx of
                                                    Nothing -> Nothing
                                                    Just (_,_,Nothing) -> Nothing
                                                    Just (_,_,Just cx') -> resolver rest cx'
        in case msum . map (resolver nameIn) $ context of
             Nothing -> rerr rMsg
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

-}

{-

  Service {
    name: "TestServiceWithCustomOptions"
    method {
      name: "Foo"
      input_type: ".protobuf_unittest.CustomOptionFooRequest"
      output_type: ".protobuf_unittest.CustomOptionFooResponse"
      options {
        7890860: 2
      }
    }
    options {
      7887650: 19753086419
    }
  }

7887650 sint64 for service_opt1 is not decoded by decode_raw
extend google.protobuf.ServiceOptions { optional sint64 service_opt1 = 7887650; }
service TestServiceWithCustomOptions { option (service_opt1) = -9876543210; ... }
-    3 {
-      7887650: 19753086419
-    }
+    3: "\221\262\213\036\323\333\200\313I"

-9876543210 <=> (2*9876543210)-1 <=> 19753086419

\221\262\213\036\323\333\200\313I is octal escaped, 128 bit encoded in two pieces
\221\262\213\036 => 788650*8 + 1
\323\333\200\313I where 'I' is decimal 73 => 19753086419
expected a ServiceOptions message (field 3 of a ServiceDescriptorProto)
  which should be prefix-length encoded with a single (field*8+wiretype) tag and the sint64 data
  All three should be 128 bit encoded on the wire.

7739036 differ on int32 with decode raw, message_opt1 :
extend google.protobuf.MessageOptions { optional int32 message_opt1 = 7739036; }
message TestMessageWithCustomOptions { option (message_opt1) = -56; ... }
-      7739036: 18446744073709551560
+      7739036: 4294967240

Those are Int64 <=> Word64 and Int32 <=> Word32 for (-56)

message CustomOptionOtherValues { optional (enum_opt) = TEST_OPTION_ENUM_TYPE2 }
 is decoding wrong, where
      optional DummyMessageContainingEnum.TestEnumType enum_opt = 7673233;
         where TEST_OPTION_ENUM_TYPE2 = -23
-        2: 18446744073709551593
+        2: 4294967273

Those are Int64 <=> Word64 and Int32 <=> Word32 for (-23)


-}

{-
message CustomOptionOtherValues {
  option  (int32_opt) = -100;  // To test sign-extension.
  option  (float_opt) = 12.3456789;
  option (double_opt) = 1.234567890123456789;
  option (string_opt) = "Hello, \"World\"";
  option  (bytes_opt) = "Hello\0World";
  option   (enum_opt) = TEST_OPTION_ENUM_TYPE2;
}

   4 {
     1: "CustomOptionOtherValues"
-    7 {
-      7705709: 18446744073709551516
-      7675390: 0x414587e7
-      7675390: 0x414587e7
-      7673293: 0x3ff3c0ca428c59fb
-      7673293: 0x3ff3c0ca428c59fb
-      7673285: "Hello, \"World\""
-      7673238: "Hello\000World"
-      7673233: 18446744073709551593
-    }
+    7: "\210\331\242\035\351\262\331\242\035\014Hello\\0World\252\334\242\035\020Hello, \\\"World\\\"\351\334\242\035\373Y\214B\312\300\363?\365\337\243\035\347\207EA\350\306\262\035\234\377\377\377\017"
   }

\210\331\242\035 is 7673233*8 + 0 (varint)
\351 is decimal 233 is (-23 :: Int8)
\262\331\242\035 is 7673238*8 + 2 (varint lengthencoded)
\014 is the length decimal 12 of the next string
Hello\\0World
\252\334\242\035 is 7673285*8 + 2 (varint lengthencoded)
\020 is the length decimal 16 of the next string
Hello, \\\"World\\\"
\351\334\242\035 is 7673293*8 + 1 (64 bits)
\373Y\214B\312\300\363? where Y is decimal 89, B is decimal 66, ? is decimal 63
\365\337\243\035 is 7675390*8 + 5 (32 bits)
\347\207EA where E is decimal 69, A is decimal 65
\350\306\262\035 is 7705709*8 + 0 (varint)
\234\377\377\377\017"

The first one looks like a failure of encoding 
extend google.protobuf.MessageOptions {
  ...
  optional DummyMessageContainingEnum.TestEnumType enum_opt = 7673233;
  ...
}

message DummyMessageContainingEnum {
  enum TestEnumType {
    TEST_OPTION_ENUM_TYPE1 = 22;
    TEST_OPTION_ENUM_TYPE2 = -23;
  }
}

The negative enum value should have been 


-}

{-

   4 {
     1: "VariousComplexOptions"
-    7 {
-      7646756 {
-        1: 42
-      }
-      7646756 {
-        7663707: 324
-      }
-      7646756 {          -- check optional protobuf_unittest.ComplexOptionType1 complex_opt1 = 7646756;
-        7663442 {        -- check optional ComplexOptionType3 corge = 7663442;
-          1: 876         -- check optional int32 qux = 1;
-        }
-      }
-      7636949 {
-        2: 987
-      }
-      7636949 {
-        7650927: 654
-      }
-      7636949 {
-        1 {
-          1: 743
-        }
-      }
-      7636949 {
-        1 {
-          7663707: 1999
-        }
-      }
-      7636949 {
-        1 {
-          7663442 {
-            1: 2008
-          }
-        }
-      }
-      7636949 {
-        7649992 {
-          1: 741
-        }
-      }
-      7636949 {
-        7649992 {
-          7663707: 1998
-        }
-      }
-      7636949 {
-        7649992 {
-          7663442 {
-            1: 2121
-          }
-        }
-      }
-      7636949 {
-        3 {
-          1: 321
-        }
-      }
-      7633546 {
-        1: 1971
-      }
-      7636463 {
-        1: 9
-      }
-      7636463 {
-        2 {
-          3: 22
-        }
-      }
-      7595468 {      -- check optional group ComplexOpt6 = 7595468 {optional int32 xyzzy = 7593951; }
-        7593951: 24  
-      }
-    }
+    7: "\343\334\374\034\370\375\373\034\030\371\375\373\034\322\250\217\035\003\010\263\017\372\336\220\035\004\023\030\026\031\252\375\220\035\005\032\003\010\301\002\242\342\225\035\010\222\365\235\035\003\010\354\006"
   }

\343\334\374\034 is 7595468*8 + 3 Group start
\370\375\373\034 is 7593951*8 + 0 varint (int32)
\030 is decimal 24
\371\375\373\034 is 7593951*8 + 1 wrong wiretag, should be 7595468*8 + 4 group stop
\322\250\217\035\003\010\263\017\372\336\220\035\004\023\030\026\031\252\375\220\035\005\032\003\010\301\002
\242\342\225\035 is 7646756*8 + 2
\010 is decimal length 8
  message content is:
    \222\365\235\035 is 7663442*8 + 2
    \003 is decimal length 3
      message content is:
        \010 is 1*8+0
        \354\006 is 876

-}