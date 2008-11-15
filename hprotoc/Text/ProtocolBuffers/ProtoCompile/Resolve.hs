-- | This huge module handles the loading and name resolution.  The
-- loadProto command recursively gets all the imported proto files.
-- The makeNameMaps command makes the translator from proto name to
-- Haskell name.  Many possible errors in the proto data are caught
-- and reported by these operations.
module Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto,makeNameMap,makeNameMaps,getTLS
                                                ,Env(..),TopLevel(..),ReMap,NameMap(..),LocalFP(..),CanonFP(..)) where

import qualified Text.DescriptorProtos.DescriptorProto                as D(DescriptorProto)
import qualified Text.DescriptorProtos.DescriptorProto                as D.DescriptorProto(DescriptorProto(..))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D(ExtensionRange(ExtensionRange))
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as D.ExtensionRange(ExtensionRange(..))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D(EnumDescriptorProto(EnumDescriptorProto))
import qualified Text.DescriptorProtos.EnumDescriptorProto            as D.EnumDescriptorProto(EnumDescriptorProto(..))
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumValueDescriptorProto       as D.EnumValueDescriptorProto(EnumValueDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D(FieldDescriptorProto)
import qualified Text.DescriptorProtos.FieldDescriptorProto           as D.FieldDescriptorProto(FieldDescriptorProto(..))
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type      as D(Type)
import           Text.DescriptorProtos.FieldDescriptorProto.Type      as D.Type(Type(..))
import qualified Text.DescriptorProtos.FileDescriptorProto            as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto            as D.FileDescriptorProto(FileDescriptorProto(..))
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
import Data.List(foldl',stripPrefix,lookup,isPrefixOf)
import Data.Map(Map)
import Data.Maybe(mapMaybe)
import Data.Monoid(Monoid(..))
import Data.Set(Set)
import System.Directory
import qualified System.FilePath as Local(pathSeparator,splitDirectories,joinPath,combine,makeRelative)
import qualified System.FilePath.Posix as Canon(pathSeparator,splitDirectories,joinPath,takeBaseName)
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

annErr :: (MonadError String m) => String -> m a -> m a
annErr s act = catchError act (\e -> throwError ("annErr: "++s++'\n':indent e))

getJust :: (Error e,MonadError e m, Typeable a) => String -> Maybe a -> m a
{-#  INLINE getJust #-}
getJust s ma@Nothing = throw $ "Impossible? Expected Just of type "++show (typeOf ma)++" but got nothing:\n"++indent s
getJust _s (Just a) = return a

-- This adds a leading dot if the input is non-empty
joinDot :: [IName String] -> FIName String
joinDot [] = err $ "joinDot on an empty list of IName!"
joinDot (x:xs) = fqAppend (promoteFI x) xs

checkFI :: [(FieldId,FieldId)] -> FieldId -> Bool
checkFI ers fid = any (`inRange` fid) ers

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

newtype LocalFP = LocalFP { unLocalFP :: FilePath } deriving (Read,Show,Eq,Ord)
newtype CanonFP = CanonFP { unCanonFP :: FilePath } deriving (Read,Show,Eq,Ord)

fpLocalToCanon :: LocalFP -> CanonFP
fpLocalToCanon | Canon.pathSeparator == Local.pathSeparator = CanonFP . unLocalFP
               | otherwise = CanonFP . Canon.joinPath . Local.splitDirectories . unLocalFP

fpCanonToLocal :: CanonFP -> LocalFP
fpCanonToLocal | Canon.pathSeparator == Local.pathSeparator = LocalFP . unCanonFP
               | otherwise = LocalFP . Local.joinPath . Canon.splitDirectories . unCanonFP

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

makeNameMaps :: [MName String] -> [(CanonFP,[MName String])] -> Env -> Either ErrStr NameMap
makeNameMaps hPrefix hAs env = do
  let getPrefix fdp =
        let ans = case D.FileDescriptorProto.name fdp of
                    Nothing -> hPrefix -- really likely to be an error elsewhere
                    Just n -> let path = CanonFP (toString n)
                              in case lookup path hAs of
                                    Just p -> p
                                    Nothing -> case lookup (CanonFP . Canon.takeBaseName . unCanonFP $ path) hAs of
                                                 Just p -> p
                                                 Nothing -> hPrefix
        in ans 
  let (tl,tls) = getTLS env
  (fdp:fdps) <- mapM top'FDP (tl:tls)
  (NameMap tuple m) <- makeNameMap (getPrefix fdp) fdp
  let f (NameMap _ x) = x
  ms <- fmap (map f) . mapM (\y -> makeNameMap (getPrefix y) y) $ fdps
  return (NameMap tuple (M.unions (m:ms)))

-- | 'makeNameMap' conservatively checks its input.
makeNameMap :: [MName String] -> D.FileDescriptorProto -> Either ErrStr NameMap
makeNameMap hPrefix fdpIn = -- trace (show ("getPrefix",D.FileDescriptorProto.name fdpIn,hPrefix)) $
                            go (makeOne fdpIn) where
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
    runReaderT (mrmFile fdp) template
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
-- trace (unlines [show fqSelf,ishow self]) $ 
  return template'

getNames :: String -> (a -> Maybe Utf8) -> a -> SE (IName String,[IName String])
getNames errorMessage accessor record = do
  parent <- asks my'Parent
  iSelf <- getJust errorMessage (validI =<< accessor record)
  let names = parent ++ [ iToString iSelf ]
  return (iToString iSelf,names)

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
      ef' = foldl' (\m (k,v) -> seq v $ M.insertWithKey mergeWires k v m) ef ios

      mergeWires k (ExtFromWire wt1 newData) (ExtFromWire wt2 oldData) =
        if wt1 /= wt2 then err $ "interpretOptions.mergeWires : ExtFromWire WireType mismatch while storing new options in extension fields: " ++ show (name,k,(wt1,wt2))
          else ExtFromWire wt2 (mappend oldData newData)
      mergeWires k a b = err $ "interpretOptions.mergeWires : impossible case\n"++show (k,a,b)
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
              Just enumIVal -> case M.lookup enumIVal enumVals of
                                 Nothing -> iFail $ "enumVal lookup failed: "++show (enumIVal,M.keys enumVals)
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

-- | 'findFile' looks through the current and import directories to find the target file on the system.
-- It also converts the relative path to a standard form to use as the name of the FileDescriptorProto.
findFile :: [LocalFP] -> LocalFP -> IO (Maybe (LocalFP,CanonFP)) -- absolute and canonical parts
findFile paths (LocalFP target) = test paths where
  test [] = return Nothing
  test (LocalFP path:rest) = do
    let fullname = Local.combine path target
    found <- doesFileExist fullname -- stop at first hit
    if not found
      then test rest
      else do truepath <- canonicalizePath path
              truefile <- canonicalizePath fullname
              if truepath `isPrefixOf` truefile
                then do let rel = fpLocalToCanon (LocalFP (Local.makeRelative truepath truefile))
                        return (Just (LocalFP truefile,rel))
                else fail $ "file found but it is not below path, cannot make canonical name:\n  path: "
                            ++show truepath++"\n  file: "++show truefile

loadProto :: [LocalFP] -> LocalFP -> IO (Env,[D.FileDescriptorProto])
loadProto protoDirs protoFile = goState (load Set.empty protoFile) where
  goState act = do (env,m) <- runStateT act mempty
                   let fromRight (Right x) = x
                       fromRight (Left s) = error $ "loadProto failed to resolve a FileDescriptorProto: "++s
                   return (env,map (fromRight . top'FDP . fst . getTLS) (M.elems m))
  loadFailed f msg = fail . unlines $ ["Parsing proto:",show (unLocalFP f),"has failed with message",msg]
  load :: Set.Set LocalFP -> LocalFP -> StateT (Map LocalFP Env) IO Env
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
          Nothing -> loadFailed file (unlines (["loading failed, could not find file: "++show (unLocalFP file)
                                               ,"Searched paths were:"] ++ map (("  "++).show.unLocalFP) protoDirs))
          Just (toRead,relpath) -> do
            protoContents <- liftIO $ do print ("Loading filepath: "++show (unLocalFP toRead))
                                         LC.readFile (unLocalFP toRead)
            parsed'fdp <- either (loadFailed toRead . show) return $
                          (parseProto (unCanonFP relpath) protoContents)
            packageName <- either (loadFailed toRead . show) (return . map iToString . snd) $
                           (checkDIUtf8 =<< getJust "makeTopLevel.packageName" (D.FileDescriptorProto.package parsed'fdp))
            let parents = Set.insert file parentsIn
                importList = map (fpCanonToLocal . CanonFP . toString) . F.toList . D.FileDescriptorProto.dependency $ parsed'fdp
            imports <- mapM (fmap getTL . load parents) importList
            let eEnv = makeTopLevel parsed'fdp packageName imports
            global'env <- either (loadFailed file) return eEnv
            either (loadFailed file) return (top'FDP . getTL $ global'env)
            modify (M.insert file global'env) -- add to memorized results
            return global'env
