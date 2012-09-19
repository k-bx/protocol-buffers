{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
{-  fixing resolution.  This is a large beast of a module.  Sorry.
  updated for version 2.0.3 to match protoc's namespace resolution better
  updated for version 2.0.4 to differentiate Entity and E'Entity, this makes eName a total selector
  updated after version 2.0.5 to fix problem when package name was not specified in proto file.
    main calls either runStandalone or runPlugin which call loadProto or loadStandalone which both call loadProto'
      loadProto' uses getPackage to make packageName, loads the imports, and passes all this to makeTopLevel to get Env

        The "load" loop in loadProto' caches the imported TopLevel based on _filename_
           files can be loaded via multiple paths but this is not important
           this may interact badly with absent "package" declarations that act as part of importing package
             need these to be "polymorphic" in the packageID somehow?

        Speculate: makeTopLevel knows the parent from the imports:
           parent with explicit package could resolve "polymorphic" imports by a recursive transformation?
           parent with no explicit package could do nothing.
           root will need default explicit package name ? or special handling in loadProto' or load* ?

    Then loadProto or loadStandalone both call run' which calls makeNameMaps with the Env from loadProto'
      makeNameMaps calls makeNameMap on each top level fdp from each TopLevel in the Global Env from loadProto'
        makeNameMap calls getPackage to form packageName, and unless overridden it is also used for hParent

          makeNameMap on the imports gets called without any special knowledge of the "parent".  If
          root or some imports are still "polymorphic" then this is most annoying.

    Alternative solution: a middle step after makeTopLevel and before makeNameMaps examines and fixes all the polymorphic imports.

    The nameMap this computes is passed by run' to makeProtoInfo from MakeReflections

    The bug is being reported by main>runStandalon>loadStandalone>loadProto'>makeTopLevel>resolveFDP>fqFileDP>fqMessage>fqField>resolvePredEnv
    

  entityField uses resolveMGE instead of expectMGE and resolveEnv : this should allow field types to resolve just to MGE insteadof other field names.

  what about keys 'extendee' resolution to Message names only? expectM in entityField

  'makeTopLevel' is the main internal entry point in this module.
  This is called from loadProto' which has two callers:
  loadProto and loadCodeGenRequest

  makeTopLevel uses a lazy 'myFixSE' trick and so the order of execution is not immediately clear.

  The environment for name resolution comes from the global' declaration which first involves using
  resolveFDP/runRE (E'Entity).  To make things more complicated the definition of global' passes
  global' to (resolveFDP fdp).

  The resolveFDP/runRE runs all the fq* stuff (E'Entity and consumeUNO/interpretOption/resolveHere).

  Note that the only source of E'Error values of E'Entity are from 'unique' detecting name
  collisions.

  This global' environment gets fed back in as global'Param to form the SEnv for running the
  entityMsg, entityField, entityEnum, entityService functions.  These clean up the parsed descriptor
  proto structures into dependable and fully resolved ones.

  The kids operator and the unZip are used to seprate and collect all the error messages, so that
  they can be checked for and reported as a group.

  ====

  Problem? Nesting namespaces allows shadowing.  I forget if Google's protoc allows this.

  Problem? When the current file being resolves has the same package name as an imported file then
   hprotoc will find unqualified names in the local name space and the imported name space.  But if
   there is a name collision between the two then hprotoc will not detect this; the unqualified name
   will resolve to the local file and not observe the duplicate from the import.  TODO: check what
   Google's protoc does in this case.

  Solution to either of the above might be to resolve to a list of successes and check for a single
  success.  This may be too lazy.

  ====

  aggregate option types not handled: Need to take fk and bytestring from parser and:
    1) look at mVal of fk (E'Message) to understand what fields are expected (listed in mVals of this mVal).
    2) lex the bytestring
    3) parse the sequence of "name" ":" "value" by doing
      4) find "name" in the expected list from (1) (E'Field)
      5) Look at the fType of this E'Field and parse the "value", if Nothing (message/group/enum) then
         6) resolve name and look at mVal
         7) if enum then parse "value" as identifier or if message or group
            8) recursively go to (1) and either prepend lenght (message) or append stop tag (group)
      9) runPut to get the wire encoded field tag and value when Just a simple type
    10) concatentanate the results of (3) to get the wire encoding for the message value

  Handling recursive message/groups makes this more annoying.

-}

-- | This huge module handles the loading and name resolution.  The
-- loadProto command recursively gets all the imported proto files.
-- The makeNameMaps command makes the translator from proto name to
-- Haskell name.  Many possible errors in the proto data are caught
-- and reported by these operations.
--
-- hprotoc will actually resolve more unqualified imported names than Google's protoc which requires
-- more qualified names.  I do not have the obsessive nature to fix this.
module Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto,loadCodeGenRequest,makeNameMaps,getTLS,getPackageID
                                                ,Env(..),TopLevel(..),ReMap,NameMap(..),PackageID(..),LocalFP(..),CanonFP(..)) where

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
import           Text.DescriptorProtos.FieldDescriptorProto.Label
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
-- import qualified Text.DescriptorProtos.EnumOptions      as D(EnumOptions)
import qualified Text.DescriptorProtos.EnumOptions      as D.EnumOptions(EnumOptions(uninterpreted_option))
-- import qualified Text.DescriptorProtos.EnumValueOptions as D(EnumValueOptions)
import qualified Text.DescriptorProtos.EnumValueOptions as D.EnumValueOptions(EnumValueOptions(uninterpreted_option))
import qualified Text.DescriptorProtos.FieldOptions     as D(FieldOptions(FieldOptions))
import qualified Text.DescriptorProtos.FieldOptions     as D.FieldOptions(FieldOptions(packed,uninterpreted_option))
-- import qualified Text.DescriptorProtos.FileOptions      as D(FileOptions)
import qualified Text.DescriptorProtos.FileOptions      as D.FileOptions(FileOptions(..))
-- import qualified Text.DescriptorProtos.MessageOptions   as D(MessageOptions)
import qualified Text.DescriptorProtos.MessageOptions   as D.MessageOptions(MessageOptions(uninterpreted_option))
-- import qualified Text.DescriptorProtos.MethodOptions    as D(MethodOptions)
import qualified Text.DescriptorProtos.MethodOptions    as D.MethodOptions(MethodOptions(uninterpreted_option))
-- import qualified Text.DescriptorProtos.ServiceOptions   as D(ServiceOptions)
import qualified Text.DescriptorProtos.ServiceOptions   as D.ServiceOptions(ServiceOptions(uninterpreted_option))

import qualified Text.Google.Protobuf.Compiler.CodeGeneratorRequest as CGR

import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.ProtoCompile.Instances
import Text.ProtocolBuffers.ProtoCompile.Parser

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Writer
import Data.Char
import Data.Ratio
import Data.Ix(inRange)
import Data.List(foldl',stripPrefix,isPrefixOf,isSuffixOf)
import Data.Map(Map)
import Data.Maybe(mapMaybe)
import Data.Typeable
-- import Data.Monoid()
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
errMsg s = "Text.ProtocolBuffers.ProtoCompile.Resolve fatal error encountered, message:\n"++indent s

err :: forall b. String -> b
err = error . errMsg 

throw :: (Error e, MonadError e m) =>  String -> m a
throw s = throwError (strMsg (errMsg s))

annErr :: (MonadError String m) => String -> m a -> m a
annErr s act = catchError act (\e -> throwError ("Text.ProtocolBuffers.ProtoCompile.Resolve annErr: "++s++'\n':indent e))

getJust :: (Error e,MonadError e m, Typeable a) => String -> Maybe a -> m a
{-#  INLINE getJust #-}
getJust s ma@Nothing = throw $ "Impossible? Expected Just of type "++show (typeOf ma)++" but got nothing:\n"++indent s
getJust _s (Just a) = return a

defaultPackageName :: Utf8
defaultPackageName = Utf8 (LC.pack "defaultPackageName")

-- The "package" name turns out to be more complicated than I anticipated (when missing).  Instead
-- of plain UTF8 annotate this with the PackageID newtype to force me to trace the usage.  Later
-- change this to track the additional complexity.
--newtype PackageID a = PackageID { getPackageID :: a } deriving (Show)

data PackageID a = PackageID { _getPackageID :: a }
                 | NoPackageID { _getNoPackageID :: a }
 deriving (Show)

instance Functor PackageID where
   fmap f (PackageID a) = PackageID (f a)
   fmap f (NoPackageID a) = NoPackageID (f a)

-- Used in MakeReflections.makeProtoInfo
getPackageID :: PackageID a -> a
getPackageID (PackageID a) = a
getPackageID (NoPackageID a) = a

-- The package field of FileDescriptorProto is set in Parser.hs.
-- 'getPackage' is the only direct user of this information in hprotoc.
-- The convertFileToPackage was developed looking at the Java output of Google's protoc.
-- In 2.0.5 this has lead to problems with the stricter import name resolution when the imported file has no package directive.
-- I need a fancier way of handling this.
getPackage :: D.FileDescriptorProto -> PackageID Utf8
getPackage fdp = case D.FileDescriptorProto.package fdp of
                   Just a -> PackageID a
                   Nothing -> case D.FileDescriptorProto.name fdp of
                                Nothing -> NoPackageID defaultPackageName
                                Just filename -> case convertFileToPackage filename of
                                                   Nothing -> NoPackageID defaultPackageName
                                                   Just name -> NoPackageID name

--getPackageUtf8 :: PackageID Utf8 -> Utf8
--getPackageUtf8 (PackageID {_getPackageID=x}) = x
--getPackageUtf8 (NoPackageID {_getNoPackageID=x}) = x

-- LOSES PackageID vs NoPackageID 2012-09-19
checkPackageID :: PackageID Utf8 -> Either String (PackageID (Bool,[IName Utf8]))
checkPackageID (PackageID a) = fmap PackageID (checkDIUtf8 a)
checkPackageID (NoPackageID a) = fmap NoPackageID (checkDIUtf8 a)

-- | 'convertFileToPackage' mimics what I observe protoc --java_out do to convert the file name to a
-- class name.
convertFileToPackage :: Utf8 -> Maybe Utf8
convertFileToPackage filename =
  let full = toString filename
      suffix = ".proto"
      noproto = if suffix `isSuffixOf` full then take (length full - length suffix) full else full
      convert :: Bool -> String -> String
      convert _ [] = []
      convert toUp (x:xs) | inRange ('a','z') x = if toUp
                                                    then toUpper x : convert False xs
                                                    else x : convert False xs
                          | inRange ('A','Z') x = x : convert False xs
                          | inRange ('0','9') x = x : convert True xs
                          | '_' == x = x : convert True xs
                          | otherwise = convert True xs
      converted = convert True noproto
      leading = case converted of
                  (x:_) | inRange ('0','9') x -> "proto_" ++ converted
                  _ -> converted
  in if null leading then Nothing else (Just (fromString leading))

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
data Env = Local [IName String] EMap {- E'Message,E'Group,E'Service -} Env
         | Global TopLevel [TopLevel]
  deriving Show

-- | TopLevel corresponds to all items defined in a .proto file. This
-- includes the FileOptions since this will be consulted when
-- generating the Haskell module names, and the imported files are only
-- known through their TopLevel data.
data TopLevel = TopLevel { top'Path :: FilePath
                         , top'Package :: PackageID [IName String]
                         , top'FDP :: Either ErrStr D.FileDescriptorProto -- resolvedFDP'd
                         , top'mVals :: EMap } deriving Show

-- | The EMap type is a local namespace attached to an entity
--
-- The E'Error values come from using unique to resolse name collisions when building EMap
type EMap = Map (IName String) E'Entity

-- | An Entity is some concrete item in the namespace of a proto file.
-- All Entity values have a leading-dot fully-qualified with the package "eName".
-- The E'Message,Group,Service have EMap namespaces to inner Entity items.
data Entity = E'Message { eName :: [IName String], validExtensions :: [(FieldId,FieldId)]
                                                 , mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }

            | E'Group   { eName :: [IName String], mVals :: EMap {- E'Message,Group,Field,Key,Enum -} }

            | E'Service { eName :: [IName String], mVals :: EMap {- E'Method -} }

            | E'Key     { eName :: [IName String], eMsg :: Either ErrStr Entity         {- E'Message -}
                                                 , fNumber :: FieldId, fType :: Maybe D.Type
                                                 , mVal :: Maybe (Either ErrStr Entity) {- E'Message,Group,Enum -} }

            | E'Field   { eName :: [IName String], fNumber :: FieldId, fType :: Maybe D.Type
                                                 , mVal :: Maybe (Either ErrStr Entity) {- E'Message,Group,Enum -} }

            | E'Enum    { eName :: [IName String], eVals :: Map (IName Utf8) Int32 }

            | E'Method  { eName :: [IName String], eMsgIn,eMsgOut :: Maybe (Either ErrStr Entity) {- E'Message -} }
  deriving (Show)

-- This type handles entity errors by storing them rather than propagating or throwing them.
--
-- The E'Error values come from using unique to resolse name collisions when building EMap
data E'Entity = E'Ok Entity
              | E'Error String [E'Entity]
  deriving (Show)

newtype LocalFP = LocalFP { unLocalFP :: FilePath } deriving (Read,Show,Eq,Ord)
newtype CanonFP = CanonFP { unCanonFP :: FilePath } deriving (Read,Show,Eq,Ord)

fpLocalToCanon :: LocalFP -> CanonFP
fpLocalToCanon | Canon.pathSeparator == Local.pathSeparator = CanonFP . unLocalFP
               | otherwise = CanonFP . Canon.joinPath . Local.splitDirectories . unLocalFP

fpCanonToLocal :: CanonFP -> LocalFP
fpCanonToLocal | Canon.pathSeparator == Local.pathSeparator = LocalFP . unCanonFP
               | otherwise = LocalFP . Local.joinPath . Canon.splitDirectories . unCanonFP

-- Used to create optimal error messages
allowedGlobal :: Env -> [(PackageID [IName String],[IName String])]
allowedGlobal (Local _ _ env) = allowedGlobal env
allowedGlobal (Global t ts) = map allowedT (t:ts)

allowedT :: TopLevel -> (PackageID [IName String], [IName String])
allowedT tl = (top'Package tl,M.keys (top'mVals tl))

-- Used to create optional error messages
allowedLocal :: Env -> [([IName String],[IName String])]
allowedLocal (Global _t _ts) = []
allowedLocal (Local name vals env) = allowedE : allowedLocal env
  where allowedE :: ([IName String], [IName String])
        allowedE = (name,M.keys vals)

-- Create a mapping from the "official" name to the Haskell hierarchy mangled name
type ReMap = Map (FIName Utf8) ProtoName

data NameMap = NameMap ( PackageID (FIName Utf8) -- packageName from 'getPackage' on fdp
                       , [MName String]   -- hPrefix from command line
                       , [MName String])  -- hParent from java_outer_classname, java_package, or 'getPackage'
                       ReMap
  deriving (Show)

type RE a = ReaderT Env (Either ErrStr) a


data SEnv = SEnv { my'Parent :: [IName String]   -- top level value is derived from PackageID
                 , my'Env :: Env }
--                 , my'Template :: ProtoName }

-- E'Service here is arbitrary
emptyEntity :: Entity
emptyEntity = E'Service [IName "emptyEntity from myFix"] mempty

emptyEnv :: Env
emptyEnv = Global (TopLevel "emptyEnv from myFix" (PackageID [IName "emptyEnv form myFix"]) (Left "emptyEnv: top'FDP does not exist") mempty) []

instance Show SEnv where
  show (SEnv p e) = "(SEnv "++show p++" ; "++ whereEnv e ++ ")" --" ; "++show (haskellPrefix t,parentModule t)++ " )"

type ErrStr = String

type SE a = ReaderT SEnv (Either ErrStr) a

runSE :: SEnv -> SE a -> Either ErrStr a
runSE sEnv m = runReaderT m sEnv

fqName :: Entity -> FIName Utf8
fqName = fiFromString . joinDot . eName

fiFromString :: FIName String -> FIName Utf8
fiFromString = FIName . fromString . fiName

iToString :: IName Utf8 -> IName String
iToString = IName . toString . iName

-- Three entities provide child namespaces: E'Message, E'Group, and E'Service
get'mVals'E :: E'Entity -> Maybe EMap
get'mVals'E (E'Ok entity) = get'mVals entity
get'mVals'E (E'Error {}) = Nothing

get'mVals :: Entity -> Maybe EMap
get'mVals (E'Message {mVals = x}) = Just x
get'mVals (E'Group   {mVals = x}) = Just x
get'mVals (E'Service {mVals = x}) = Just x
get'mVals _ = Nothing

-- | This is a helper for resolveEnv
toGlobal :: Env -> Env
toGlobal (Local _ _ env) = toGlobal env
toGlobal x@(Global {}) = x

getTL :: Env -> TopLevel
getTL (Local _ _ env) = getTL env
getTL (Global tl _tls) = tl

getTLS :: Env -> (TopLevel,[TopLevel])
getTLS (Local _ _ env) = getTLS env
getTLS (Global tl tls) = (tl, tls)

-- | This is used for resolving some UninterpretedOption names
resolveHere :: Entity -> Utf8 -> RE Entity
resolveHere parent nameU = do
  let rFail msg = throw ("Could not lookup "++show (toString nameU)++"\n"++indent msg)
  x <- getJust ("resolveHere: validI nameU failed for "++show nameU) (fmap iToString (validI nameU))
  case get'mVals parent of
    Just vals -> case M.lookup x vals of
                   Just (E'Ok entity) -> return entity
                   Just (E'Error s _) -> rFail ("because the name resolved to an error:\n" ++ indent s)
                   Nothing -> rFail ("because there is no such name here:  "++show (eName parent))
    Nothing -> rFail ("because environment has no local names:\n"++ishow (eName parent))

-- | 'resolvePredEnv' is the query operation for the Env namespace.  It recognizes names beginning
-- with a '.' as already being fully-qualified names. This is called from the different monads via
-- resolveEnv, resolveMGE,  and resolveM
--
-- The returned (Right _::Entity) will never be an E'Error, which results in (Left _::ErrStr) instead
resolvePredEnv :: String -> (E'Entity -> Bool) -> Utf8 -> Env -> Either ErrStr Entity
resolvePredEnv userMessage accept nameU envIn = do
  (isGlobal,xs) <- checkDIUtf8 nameU
  let mResult = if isGlobal then lookupEnv (map iToString xs) (toGlobal envIn)
                            else lookupEnv (map iToString xs) envIn
  case mResult of
    Just (E'Ok e) -> return e
    Just (E'Error s _es) -> throw s
    Nothing -> throw . unlines $ [ "resolvePredEnv: Could not lookup " ++ show nameU
                                 , "which parses as "                  ++ show (isGlobal,xs)
                                 , "in environment: "                  ++ (whereEnv envIn)
                                 , "looking for: "                     ++ userMessage
                                 , "allowed (local):  "                ++ show (allowedLocal envIn)
                                 , "allowed (global): "                ++ show (allowedGlobal envIn) ]
 where
  lookupEnv :: [IName String] -> Env -> Maybe E'Entity
  lookupEnv xs (Global tl tls) = let findThis = lookupTopLevel main xs
                                       where main = top'Package tl
                                 in msum (map findThis (tl:tls))
  lookupEnv xs (Local _ vals env) = filteredLookup vals xs <|> lookupEnv xs env

  lookupTopLevel :: PackageID [IName String] -> [IName String] -> TopLevel -> Maybe E'Entity
  lookupTopLevel main xs tl = 
    (if matchesMain main (top'Package tl) then filteredLookup (top'mVals tl) xs else Nothing)
    <|>
    (matchPrefix (top'Package tl) xs >>= filteredLookup (top'mVals tl))
   where matchesMain (PackageID {_getPackageID=a}) (PackageID {_getPackageID=b}) = a==b
         matchesMain (NoPackageID {}) (PackageID {})   = False  -- XXX XXX XXX 2012-09-19 suspicious
         matchesMain (PackageID {})   (NoPackageID {}) = True
         matchesMain (NoPackageID {}) (NoPackageID {}) = True

         matchPrefix (NoPackageID {}) _ = Nothing
         matchPrefix (PackageID {_getPackageID=a}) ys = stripPrefix a ys

  filteredLookup valsIn namesIn =
    let lookupVals :: EMap -> [IName String] -> Maybe E'Entity
        lookupVals _vals [] = Nothing
        lookupVals vals [x] = M.lookup x vals
        lookupVals vals (x:xs) = do
          entity <- M.lookup x vals
          case get'mVals'E entity of
            Just vals' -> lookupVals vals' xs
            Nothing -> Nothing
        m'x = lookupVals valsIn namesIn
    in case m'x of
         Just entity | accept entity -> m'x
         _ -> Nothing

-- Used in resolveRE and getType.resolveSE.  Accepts all types and so commits to first hit, but
-- caller may reject some types later.
resolveEnv :: Utf8 -> Env -> Either ErrStr Entity
resolveEnv = resolvePredEnv "Any item" (const True)

-- resolveRE is the often used workhorse of the fq* family of functions
resolveRE :: Utf8 -> RE Entity
resolveRE nameU = lift . (resolveEnv nameU) =<< ask

-- | 'getType' is used to lookup the type strings in service method records.
getType :: Show a => String -> (a -> Maybe Utf8) -> a -> SE (Maybe (Either ErrStr Entity))
getType s f a = do
  typeU <- getJust s (f a)
  case parseType (toString typeU) of
    Just _ -> return Nothing
    Nothing -> do ee <- resolveSE typeU
                  return (Just (expectMGE ee))
 where
  -- All uses of this then apply expectMGE or expectM, so provide predicate 'skip' support.
  resolveSE :: Utf8 -> SE (Either ErrStr Entity)
  resolveSE nameU = fmap (resolveEnv nameU) (asks my'Env)

-- | 'expectMGE' is used by getType and 'entityField'
expectMGE :: Either ErrStr Entity -> Either ErrStr Entity
expectMGE ee@(Left {}) = ee
expectMGE ee@(Right e) | isMGE = ee
                       | otherwise = throw $ "expectMGE: Name resolution failed to find a Message, Group, or Enum:\n"++ishow (eName e)
  -- cannot show all of "e" because this will loop and hang the hprotoc program
  where isMGE = case e of E'Message {} -> True
                          E'Group {} -> True
                          E'Enum {} -> True
                          _ -> False

-- | This is a helper for resolveEnv and (Show SEnv) for error messages
whereEnv :: Env -> String
whereEnv (Local name _ env) = fiName (joinDot name) ++ " in "++show (top'Path . getTL $ env)
-- WAS whereEnv (Global tl _) = fiName (joinDot (getPackageID (top'Package tl))) ++ " in " ++ show (top'Path tl)
whereEnv (Global tl _) = formatPackageID ++ " in " ++ show (top'Path tl)
  where formatPackageID = case top'Package tl of
                            PackageID {_getPackageID=x} -> fiName (joinDot x)
                            NoPackageID {_getNoPackageID=y} -> show y

-- | 'partEither' separates the Left errors and Right success in the obvious way.
partEither :: [Either a b] -> ([a],[b])
partEither [] = ([],[])
partEither (Left a:xs) = let ~(ls,rs) = partEither xs
                         in (a:ls,rs)
partEither (Right b:xs) = let ~(ls,rs) = partEither xs
                          in (ls,b:rs)

-- | The 'unique' function is used with Data.Map.fromListWithKey to detect
-- name collisions and record this as E'Error entries in the map.
--
-- This constructs new E'Error values
unique :: IName String -> E'Entity -> E'Entity -> E'Entity
unique name (E'Error _ a) (E'Error _ b) = E'Error ("Namespace collision for "++show name) (a++b)
unique name (E'Error _ a) b = E'Error ("Namespace collision for "++show name) (a++[b])
unique name a (E'Error _ b) = E'Error ("Namespace collision for "++show name) (a:b)
unique name a b = E'Error ("Namespace collision for "++show name) [a,b]

maybeM :: Monad m => (x -> m a) -> (Maybe x) -> m (Maybe a)
maybeM f mx = maybe (return Nothing) (liftM Just . f) mx

-- ReaderT containing template stacked on WriterT of list of name translations stacked on error reporting
type MRM a = ReaderT ProtoName (WriterT [(FIName Utf8,ProtoName)] (Either ErrStr)) a

runMRM'Reader :: MRM a -> ProtoName -> WriterT [(FIName Utf8,ProtoName)] (Either ErrStr) a
runMRM'Reader = runReaderT

runMRM'Writer :: WriterT [(FIName Utf8,ProtoName)] (Either ErrStr) a -> Either ErrStr (a,[(FIName Utf8,ProtoName)])
runMRM'Writer = runWriterT

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

-- Compute the nameMap that determine how to translate from proto names to haskell names
-- The loop oever makeNameMap uses the (optional) package name
-- makeNameMaps is called from the run' routine in ProtoCompile.hs for both standalone and plugin use.
-- hPrefix and hAs are command line controlled options.
-- hPrefix is "-p MODULE --prefix=MODULE dotted Haskell MODULE name to use as a prefix (default is none); last flag used"
-- hAs is "-a FILEPATH=MODULE --as=FILEPATH=MODULE assign prefix module to imported prot file: --as descriptor.proto=Text"
-- Note that 'setAs' puts both the full path and the basename as keys into the association list
makeNameMaps :: [MName String] -> [(CanonFP,[MName String])] -> Env -> Either ErrStr NameMap
makeNameMaps hPrefix hAs env = do
  let getPrefix fdp =
        case D.FileDescriptorProto.name fdp of
          Nothing -> hPrefix -- really likely to be an error elsewhere since this ought to be a filename
          Just n -> let path = CanonFP (toString n)
                    in case lookup path hAs of
                          Just p -> p
                          Nothing -> case lookup (CanonFP . Canon.takeBaseName . unCanonFP $ path) hAs of
                                       Just p -> p
                                       Nothing -> hPrefix  -- this is the usual branch unless overridden on command line
  let (tl,tls) = getTLS env
  (fdp:fdps) <- mapM top'FDP (tl:tls)
  (NameMap tuple m) <- makeNameMap (getPrefix fdp) fdp
  let f (NameMap _ x) = x
  ms <- fmap (map f) . mapM (\y -> makeNameMap (getPrefix y) y) $ fdps
  let nameMap = (NameMap tuple (M.unions (m:ms)))
--  trace (show nameMap) $ 
  return nameMap

-- | 'makeNameMap' conservatively checks its input.
makeNameMap :: [MName String] -> D.FileDescriptorProto -> Either ErrStr NameMap
makeNameMap hPrefix fdpIn = go (makeOne fdpIn) where
  go = fmap ((\(a,w) -> NameMap a (M.fromList w))) . runMRM'Writer

  makeOne fdp = do
    -- Create 'template' :: ProtoName using "Text.ProtocolBuffers.Identifiers" with error for baseName
    let rawPackage = getPackage fdp :: PackageID Utf8
    _ <- lift (checkPackageID rawPackage) -- guard-like effect
{-
    -- Previously patched way of doing this
    let packageName = case D.FileDescriptorProto.package fdp of
                        Nothing -> FIName $ fromString ""
                        Just p  -> difi $ DIName p
-}
    let packageName :: PackageID (FIName Utf8)
        packageName = fmap (difi . DIName) rawPackage
        fi'package'name = getPackageID packageName
    rawParent <- getJust "makeNameMap.makeOne: impossible Nothing found" . msum $
        [ D.FileOptions.java_outer_classname =<< (D.FileDescriptorProto.options fdp)
        , D.FileOptions.java_package =<< (D.FileDescriptorProto.options fdp)
        , Just (getPackageID rawPackage)]
    diParent <- getJust ("makeNameMap.makeOne: invalid character in: "++show rawParent)
                  (validDI rawParent)
    let hParent = map (mangle :: IName Utf8 -> MName String) . splitDI $ diParent
        template = ProtoName fi'package'name hPrefix hParent
                     (error "makeNameMap.makeOne.template.baseName undefined")
    runMRM'Reader (mrmFile fdp) template
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

getNames :: String -> (a -> Maybe Utf8) -> a -> SE (IName String,[IName String])
getNames errorMessage accessor record = do
  parent <- asks my'Parent
  iSelf <- fmap iToString $ getJust errorMessage (validI =<< accessor record)
  let names = parent ++ [ iSelf ]
  return (iSelf,names)

descend :: [IName String] -> Entity -> SE a -> SE a
descend names entity act = local mutate act
  where mutate (SEnv _parent env) = SEnv parent' env'
          where parent' = names -- cannot call eName ename, will cause <<loop>> with "getNames" -- XXX revisit
                env' = Local (eName entity) (mVals entity) env

-- Run each element of (Seq x) as (f x) with same initial environment and state.
-- Then merge the output states and sort out the failures and successes.
kids :: (x -> SE (IName String,E'Entity)) -> Seq x -> SE ([ErrStr],[(IName String,E'Entity)])
kids f xs = do sEnv <- ask
               let ans = map (runSE sEnv) . map f . F.toList $ xs
               return (partEither ans)

-- | 'makeTopLevel' takes a .proto file's FileDescriptorProto and the TopLevel values of its
-- directly imported file and constructs the TopLevel of the FileDescriptorProto in a Global
-- Environment.
--
-- This goes to some lengths to be a total function with good error messages.  Errors in building
-- the skeleton of the namespace are detected and reported instead of returning the new 'Global'
-- environment.  Collisions in the namespace are only detected when the offending name is looked up,
-- and will return an E'Error entity with a message and list of colliding Entity items.  The
-- cross-linking of Entity fields may fail and this failure is stored in the corresponding Entity.
--
-- Also caught: name collisions in Enum definitions.
--
-- The 'mdo' usage has been replace by modified forms of 'mfix' that will generate useful error
-- values instead of calling 'error' and halting 'hprotoc'.
--
-- Used from loadProto'
makeTopLevel :: D.FileDescriptorProto -> PackageID [IName String] -> [TopLevel] -> Either ErrStr Env {- Global -}
makeTopLevel fdp packageName imports = do
  filePath <- getJust "makeTopLevel.filePath" (D.FileDescriptorProto.name fdp)
  let -- There should be no TYPE_GROUP in the extension list here, but to be safe:
      isGroup = (`elem` groupNames) where
        groupNamesRaw = map toString . mapMaybe D.FieldDescriptorProto.type_name
                   . filter (maybe False (TYPE_GROUP ==) . D.FieldDescriptorProto.type') 
                   $ (F.toList . D.FileDescriptorProto.extension $ fdp)
        groupNamesI = mapMaybe validI groupNamesRaw
        groupNamesDI = mapMaybe validDI groupNamesRaw  -- These fully qualified names from using hprotoc as a plugin for protoc
        groupNames = groupNamesI ++ map (last . splitDI) groupNamesDI
  (bad,global) <- myFixE ("makeTopLevel myFixE",emptyEnv) $ \ global'Param ->
    let sEnv = SEnv (get'SEnv'root'from'PackageID packageName) global'Param
    in runSE sEnv $ do
      (bads,children) <- fmap unzip . sequence $
        [ kids (entityMsg isGroup) (D.FileDescriptorProto.message_type fdp)
        , kids (entityField True)  (D.FileDescriptorProto.extension    fdp)
        , kids entityEnum          (D.FileDescriptorProto.enum_type    fdp)
        , kids entityService       (D.FileDescriptorProto.service      fdp) ]
      let bad' = unlines (concat bads)
          global' = Global (TopLevel (toString filePath)
                                     packageName
                                     (resolveFDP fdp global')
                                     (M.fromListWithKey unique (concat children)))
                           imports
      return (bad',global')
  -- Moving this outside the myFixE reduces the cases where myFixE generates an 'error' call.
  when (not (null bad)) $
    throw $ "makeTopLevel.bad: Some children failed for "++show filePath++"\n"++bad
  return global

 where resolveFDP :: D.FileDescriptorProto -> Env -> Either ErrStr D.FileDescriptorProto
       resolveFDP fdpIn env = runRE env (fqFileDP fdpIn)
        where runRE :: Env -> RE D.FileDescriptorProto -> Either ErrStr D.FileDescriptorProto
              runRE envIn m = runReaderT m envIn

       -- Used from makeTopLevel, from loadProto'
       get'SEnv'root'from'PackageID :: PackageID [IName String] -> [IName String]
       get'SEnv'root'from'PackageID = getPackageID -- was mPackageID before 2012-09-19
           -- where
           -- Used from get'SEnv  makeTopLevel, from loadProto'
           -- mPackageID :: Monoid a => PackageID a -> a
           -- mPackageID (PackageID {_getPackageID=x}) = x
           -- mPackageID (NoPackageID {}) = mempty


-- Copies of mFix for use the string in (Left msg) for the error message.
-- Note that the usual mfix for Either calls 'error' while this does not,
-- it uses a default value passed to myFix*.
myFixSE :: (String,a) -> (a -> SE (String,a)) -> SE (String,a)
myFixSE s f = ReaderT $ \r -> myFixE s (\a -> runReaderT (f a) r)

-- Note that f ignores the fst argument
myFixE :: (String,a) -> (a -> Either ErrStr (String,a)) -> Either ErrStr (String,a)
myFixE s f = let a = f (unRight a) in a
    where unRight (Right x) = snd x
          unRight (Left _msg) = snd s
--            ( "Text.ProtocolBuffers.ProtoCompile.Resolve: "++fst s ++":\n" ++ indent msg
--                               , snd s)

{- ***
All the entity* functions are used by makeTopLevel and each other.
If there is no error then these return (IName String,E'Entity) and this E'Entity is always E'Ok.
 *** -}

-- Fix this to look at groupNamesDI as well as the original list of groupNamesI.  This fixes a bug
-- in the plug-in usage because protoc will have already resolved the type_name to a fully qualified
-- name.
entityMsg :: (IName String -> Bool) -> D.DescriptorProto -> SE (IName String,E'Entity)
entityMsg isGroup dp = annErr ("entityMsg DescriptorProto name is "++show (D.DescriptorProto.name dp)) $ do
  (self,names) <- getNames "entityMsg.name" D.DescriptorProto.name dp
  numbers <- fmap Set.fromList . mapM (getJust "entityMsg.field.number" . D.FieldDescriptorProto.number) . F.toList . D.DescriptorProto.field $ dp
  when (Set.size numbers /= Seq.length (D.DescriptorProto.field dp)) $
    throwError $ "entityMsg.field.number: There must be duplicate field numbers for "++show names++"\n "++show numbers
  let groupNamesRaw = map toString . mapMaybe D.FieldDescriptorProto.type_name
                      . filter (maybe False (TYPE_GROUP ==) . D.FieldDescriptorProto.type') 
                      $ (F.toList . D.DescriptorProto.field $ dp) ++ (F.toList . D.DescriptorProto.extension $ dp)
      groupNamesI = mapMaybe validI groupNamesRaw
      groupNamesDI = mapMaybe validDI groupNamesRaw  -- These fully qualified names from using hprotoc as a plugin for protoc
      groupNames = groupNamesI ++ map (last . splitDI) groupNamesDI
      isGroup' = (`elem` groupNames)
  (bad,entity) <- myFixSE ("myFixSE entityMsg",emptyEntity) $ \ entity'Param -> descend names entity'Param $ do
    (bads,children) <- fmap unzip . sequence $
      [ kids entityEnum           (D.DescriptorProto.enum_type   dp)
      , kids (entityField True)   (D.DescriptorProto.extension   dp)
      , kids (entityField False)  (D.DescriptorProto.field       dp)
      , kids (entityMsg isGroup') (D.DescriptorProto.nested_type dp) ]
    let bad' = unlines (concat bads)
        entity' | isGroup self = E'Group names (M.fromListWithKey unique (concat children))
                | otherwise = E'Message names (getExtRanges dp) (M.fromListWithKey unique (concat children))
    return (bad',entity')
  -- Moving this outside the myFixSE reduces the cases where myFixSE uses the error-default call.
  when (not (null bad)) $
    throwError $ "entityMsg.bad: Some children failed for "++show names++"\n"++bad
  return (self,E'Ok $ entity)

-- Among other things, this is where ambiguous type names in the proto file are resolved into a
-- Message or a Group or an Enum.
entityField :: Bool -> D.FieldDescriptorProto -> SE (IName String,E'Entity)
entityField isKey fdp = annErr ("entityField FieldDescriptorProto name is "++show (D.FieldDescriptorProto.name fdp)) $ do
  (self,names) <- getNames "entityField.name" D.FieldDescriptorProto.name fdp
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    throwError $ "entityField: Impossible? Expected key and got field or vice-versa:\n"++ishow ((isKey,isKey'),names,fdp)
  number <- getJust "entityField.name" . D.FieldDescriptorProto.number $ fdp
  let mType = D.FieldDescriptorProto.type' fdp
  typeName <- maybeM resolveMGE (D.FieldDescriptorProto.type_name fdp)
  if isKey
    then do
      extendee <- resolveM =<< getJust "entityField.extendee" (D.FieldDescriptorProto.extendee fdp)
      return (self,E'Ok $ E'Key names extendee (FieldId number) mType typeName)
    else
      return (self,E'Ok $ E'Field names (FieldId number) mType typeName)
 where
  resolveMGE :: Utf8 -> SE (Either ErrStr Entity)
  resolveMGE nameU = fmap (resolvePredEnv "Message or Group or Enum" isMGE nameU) (asks my'Env)
    where isMGE (E'Ok e') = case e' of E'Message {} -> True
                                       E'Group {} -> True
                                       E'Enum {} -> True
                                       _ -> False
          isMGE (E'Error {}) = False
  -- To be used for key extendee name resolution, but not part of the official protobuf-2.1.0 update, since made official
  resolveM :: Utf8 -> SE (Either ErrStr Entity)
  resolveM nameU = fmap (resolvePredEnv "Message" isM nameU) (asks my'Env)
    where isM (E'Ok e') = case e' of E'Message {} -> True
                                     _ -> False
          isM (E'Error {}) = False

entityEnum :: D.EnumDescriptorProto -> SE (IName String,E'Entity)
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
  descend'Enum names $ F.mapM_ entityEnumValue vs
  return (self,E'Ok $ E'Enum names mapping) -- discard values

 where entityEnumValue :: D.EnumValueDescriptorProto -> SE ()
       entityEnumValue evdp = do -- Merely use getNames to add mangled self to ReMap state
         _ <- getNames "entityEnumValue.name" D.EnumValueDescriptorProto.name evdp
         return ()

       descend'Enum :: [IName String] -> SE a -> SE a
       descend'Enum names act = local mutate act
         where mutate (SEnv _parent env) = SEnv names env

entityService :: D.ServiceDescriptorProto -> SE (IName String,E'Entity)
entityService sdp = annErr ("entityService ServiceDescriptorProto name is "++show (D.ServiceDescriptorProto.name sdp)) $ do
  (self,names) <- getNames "entityService.name" D.ServiceDescriptorProto.name sdp
  (bad,entity) <- myFixSE ("myFixSE entityService",emptyEntity) $ \ entity'Param ->
    descend names entity'Param $ do
      (badMethods',goodMethods) <- kids entityMethod (D.ServiceDescriptorProto.method sdp)
      let bad' = unlines badMethods'
          entity' = E'Service names (M.fromListWithKey unique goodMethods)
      return (bad',entity')
  -- Moving this outside the myFixSE reduces the cases where myFixSE generates an 'error' call.
  when (not (null bad)) $
    throwError $ "entityService.badMethods: Some methods failed for "++show names++"\n"++bad
  return (self,E'Ok entity)

entityMethod :: D.MethodDescriptorProto -> SE (IName String,E'Entity)
entityMethod mdp = do
  (self,names) <- getNames "entityMethod.name" D.MethodDescriptorProto.name mdp
  inputType <- getType "entityMethod.input_type" D.MethodDescriptorProto.input_type mdp
  outputType <- getType "entityMethod.output_type" D.MethodDescriptorProto.output_type mdp
  return (self,E'Ok $ E'Method names inputType outputType)

{- ***

The namespace Env is used to transform the original FileDescriptorProto into a canonical
FileDescriptorProto. The goal is to match the transformation done by Google's protoc program.  This
will allow the "front end" vs "back end" of each program to cross-couple, which will at least allow
better testing of hprotoc and the new UninterpretedOption support.

The UninterpretedOption fields are converted by the resolveFDP code below.

These should be total functions with no 'error' or 'undefined' values possible.

*** -}

fqFail :: Show a => String -> a -> Entity -> RE b
fqFail msg dp entity = do
  env <- ask
  throw $ unlines [ msg, "resolving: "++show dp, "in environment: "++whereEnv env, "found: "++show (eName entity) ]

fqFileDP :: D.FileDescriptorProto -> RE D.FileDescriptorProto
fqFileDP fdp = annErr ("fqFileDP FileDescriptorProto (name,package) is "++show (D.FileDescriptorProto.name fdp,D.FileDescriptorProto.package fdp)) $ do
  newMessages <- T.mapM fqMessage      (D.FileDescriptorProto.message_type fdp)
  newEnums    <- T.mapM fqEnum         (D.FileDescriptorProto.enum_type    fdp)
  newServices <- T.mapM fqService      (D.FileDescriptorProto.service      fdp)
  newKeys     <- T.mapM (fqField True) (D.FileDescriptorProto.extension    fdp)
  consumeUNO $ fdp { D.FileDescriptorProto.message_type = newMessages
                   , D.FileDescriptorProto.enum_type    = newEnums
                   , D.FileDescriptorProto.service      = newServices
                   , D.FileDescriptorProto.extension    = newKeys }

fqMessage :: D.DescriptorProto -> RE D.DescriptorProto
fqMessage dp = annErr ("fqMessage DescriptorProto name is "++show (D.DescriptorProto.name dp)) $ do
  entity <- resolveRE =<< getJust "fqMessage.name" (D.DescriptorProto.name dp)
  (name,vals) <- case entity of
                   E'Message {eName=name',mVals=vals'} -> return (name',vals')
                   E'Group {eName=name',mVals=vals'} -> return (name',vals')
                   _ -> fqFail "fqMessage.entity: did not resolve to an E'Message or E'Group:" dp entity
  local (\env -> (Local name vals env)) $ do
    newFields   <- T.mapM (fqField False) (D.DescriptorProto.field       dp)
    newKeys     <- T.mapM (fqField True)  (D.DescriptorProto.extension   dp)
    newMessages <- T.mapM fqMessage       (D.DescriptorProto.nested_type dp)
    newEnums    <- T.mapM fqEnum          (D.DescriptorProto.enum_type   dp)
    consumeUNO $ dp { D.DescriptorProto.field       = newFields
                    , D.DescriptorProto.extension   = newKeys
                    , D.DescriptorProto.nested_type = newMessages
                    , D.DescriptorProto.enum_type   = newEnums }

fqService :: D.ServiceDescriptorProto -> RE D.ServiceDescriptorProto
fqService sdp =  annErr ("fqService ServiceDescriptorProto name is "++show (D.ServiceDescriptorProto.name sdp)) $ do
  entity <- resolveRE =<< getJust "fqService.name" (D.ServiceDescriptorProto.name sdp)
  case entity of
    E'Service {eName=name,mVals=vals} -> do
      newMethods <- local (Local name vals) $ T.mapM fqMethod (D.ServiceDescriptorProto.method sdp)
      consumeUNO $ sdp { D.ServiceDescriptorProto.method = newMethods }
    _ -> fqFail "fqService.entity: did not resolve to a service:" sdp entity

fqMethod :: D.MethodDescriptorProto -> RE D.MethodDescriptorProto
fqMethod mdp = do
  entity <- resolveRE =<< getJust "fqMethod.name" (D.MethodDescriptorProto.name mdp)
  case entity of
    E'Method {eMsgIn=msgIn,eMsgOut=msgOut} -> do
      mdp1 <- case msgIn of
                Nothing -> return mdp
                Just resolveIn -> do
                  new <- fmap fqName (lift resolveIn)
                  return (mdp {D.MethodDescriptorProto.input_type = Just (fiName new)})
      mdp2 <- case msgOut of
                Nothing -> return mdp1
                Just resolveIn -> do
                  new <- fmap fqName (lift resolveIn)
                  return (mdp1 {D.MethodDescriptorProto.output_type = Just (fiName new)})
      consumeUNO mdp2
    _ -> fqFail "fqMethod.entity: did not resolve to a Method:" mdp entity

-- The field is a bit more complicated to resolve.  The Key variant needs to resolve the extendee.
-- The type code from Parser.hs might be Nothing and this needs to be resolved to TYPE_MESSAGE or
-- TYPE_ENUM (at last!), and if it is the latter then any default value string is checked for
-- validity.
fqField :: Bool -> D.FieldDescriptorProto -> RE D.FieldDescriptorProto
fqField isKey fdp = annErr ("fqField FieldDescriptorProto name is "++show (D.FieldDescriptorProto.name fdp)) $ do
  let isKey' = maybe False (const True) (D.FieldDescriptorProto.extendee fdp)
  when (isKey/=isKey') $
    ask >>= \env -> throwError $ "fqField.isKey: Expected key and got field or vice-versa:\n"++ishow ((isKey,isKey'),whereEnv env,fdp)
  entity <- expectFK =<< resolveRE =<< getJust "fqField.name" (D.FieldDescriptorProto.name fdp)
  newExtendee <- case (isKey,entity) of
                   (True,E'Key {eMsg=msg,fNumber=fNum}) -> do
                      ext <- lift msg
                      case ext of
                        E'Message {} -> when (not (checkFI (validExtensions ext) fNum)) $
                          throwError $ "fqField.newExtendee: Field Number of extention key invalid:\n"
                            ++unlines ["Number is "++show (fNumber entity)
                                      ,"Valid ranges: "++show (validExtensions ext)
                                      ,"Extendee: "++show (eName ext)
                                      ,"Descriptor: "++show fdp]
                        _ -> fqFail "fqField.ext: Key's target is not an E'Message:" fdp ext
                      fmap (Just . fiName . fqName) . lift . eMsg $ entity
                   (False,E'Field {}) -> return Nothing
                   _ -> fqFail "fqField.entity: did not resolve to expected E'Key or E'Field:" fdp entity
  mTypeName <- maybeM lift (mVal entity) -- "Just (Left _)" triggers a throwError here (see comment for entityField)
  -- Finally fully determine D.Type, (type'==Nothing) meant ambiguously TYPE_MESSAGE or TYPE_ENUM from Parser.hs
  -- This has gotten more verbose with the addition of verifying packed is being used properly.
  actualType <- case (fType entity,mTypeName) of
                  (Just TYPE_GROUP, Just (E'Group {})) | isNotPacked fdp -> return TYPE_GROUP
                                                       | otherwise -> 
                    fqFail ("fqField.actualType : This Group is invalid, you cannot pack a group field.") fdp entity
                  (Nothing, Just (E'Message {})) | isNotPacked fdp -> return TYPE_MESSAGE
                                                 | otherwise ->
                    fqFail ("fqField.actualType : This Message is invalid, you cannot pack a message field.") fdp entity
                  (Nothing, Just (E'Enum {})) | isNotPacked fdp -> return TYPE_ENUM
                                              | isRepeated fdp -> return TYPE_ENUM
                                              | otherwise ->
                    fqFail ("fqField.actualType : This Enum is invalid, you cannot pack a non-repeated field.") fdp entity
                  (Just t, Nothing) -> return t
                  (Just TYPE_MESSAGE, Just (E'Message {})) -> return TYPE_MESSAGE
                  (Just TYPE_ENUM, Just (E'Enum {})) -> return TYPE_ENUM
                  (mt,me) -> fqFail ("fqField.actualType: "++show mt++" and "++show (fmap eName me)++" is invalid.") fdp entity
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

 where isRepeated :: D.FieldDescriptorProto -> Bool
       isRepeated (D.FieldDescriptorProto {
                    D.FieldDescriptorProto.label =
                      Just LABEL_REPEATED }) =
         True
       isRepeated _ = False

       isNotPacked :: D.FieldDescriptorProto -> Bool
       isNotPacked (D.FieldDescriptorProto { 
                     D.FieldDescriptorProto.options =
                      Just (D.FieldOptions { 
                             D.FieldOptions.packed =
                               Just isPacked })}) =
         not isPacked
       isNotPacked _ = True

       expectFK :: Entity -> RE Entity
       expectFK e | isFK = return e
                  | otherwise = throwError $ "expectF: Name resolution failed to find a Field or Key:\n"++ishow (eName e)
         where isFK = case e of E'Field {} -> True
                                E'Key {} -> True
                                _ -> False



fqEnum :: D.EnumDescriptorProto -> RE D.EnumDescriptorProto
fqEnum edp = do
  entity <- resolveRE =<< getJust "fqEnum.name" (D.EnumDescriptorProto.name edp)
  case entity of
    E'Enum {} -> do evdps <- T.mapM consumeUNO (D.EnumDescriptorProto.value edp)
                    consumeUNO $ edp { D.EnumDescriptorProto.value = evdps }
    _ -> fqFail "fqEnum.entity: did not resolve to an E'Enum:" edp entity

{- The consumeUNO calls above hide this cut-and-pasted boilerplate between interpretOptions and the DescriptorProto type -}

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

      mergeWires _k (ExtFromWire newData) (ExtFromWire oldData) =
          ExtFromWire (mappend oldData newData)
{-
      mergeWires k (ExtFromWire wt1 newData) (ExtFromWire wt2 oldData) =
        if wt1 /= wt2 then err $ "interpretOptions.mergeWires : ExtFromWire WireType mismatch while storing new options in extension fields: " ++ show (name,k,(wt1,wt2))
          else ExtFromWire wt2 (mappend oldData newData)
-}
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
  iFail :: String -> RE a  -- needed by ghc-7.0.2
  iFail msg = do env <- ask
                 throw $ unlines [ "interpretOption: Failed to handle UninterpretedOption for: "++show optName
                                 , "  environment: "++whereEnv env
                                 , "  value: "++show uno
                                 , "  message: "++msg ]

  -- This takes care of an intermediate message or group type
  go :: Maybe Entity {- E'Message E'Group -} -> [IName String] -> D.NamePart -> [D.NamePart] -> RE (FieldId,ExtFieldValue)
  go mParent names (D.NamePart { D.NamePart.name_part = name
                               , D.NamePart.is_extension = isKey }) (next:rest) = do
    -- get entity (Field or Key) and the TYPE_*
    -- fk will ceratinly be E'Field or E'Key
    (fk,entity) <-
      if not isKey
        then case mParent of
               Nothing -> iFail $ "Cannot resolve local (is_extension False) name, no parent; expected (key)."
               Just parent -> do
                 entity'field <- resolveHere parent name
                 case entity'field of
                   (E'Field {}) ->
                     case mVal entity'field of
                       Nothing -> iFail $ "Intermediate entry E'Field is of basic type, not E'Message or E'Group: "++show (names,eName entity'field)
                       Just val -> lift val >>= \e -> return (entity'field,e)
                   _ -> iFail $ "Name "++show (toString name)++" was resolved but was not an E'Field: "++show (eName entity'field)
        else do entity'key <- resolveRE name
                case entity'key of
                  (E'Key {eMsg=msg}) -> do
                    extendee <- lift msg
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
    (fid',ExtFromWire raw') <- go (Just entity) (eName entity) next rest
    -- wrap old tag + inner result with outer info
    let tag@(WireTag tag') = mkWireTag fid' wt'
        (EP wt' bs') = Seq.index raw' 0
    let fid = fNumber fk -- safe by construction of fk
        wt = toWireType (FieldType (fromEnum t))
        bs = runPut $
          case t of TYPE_MESSAGE -> do putSize (size'WireTag tag + LC.length bs')
                                       putVarUInt tag'
                                       putLazyByteString bs'
                    TYPE_GROUP -> do putVarUInt tag'
                                     putLazyByteString bs'
                                     putVarUInt (succ (getWireTag (mkWireTag fid wt)))
                    _ -> fail $ "bug! raw with type "++show t++" should be impossible"
    return (fid,ExtFromWire (Seq.singleton (EP wt bs)))

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
           Nothing -> return TYPE_ENUM -- XXX not a good assumption with aggregate types !!!!  This also covers groups and messages.
           Just TYPE_GROUP -> iFail $ "Last entry was a TYPE_GROUP instead of concrete value type" -- impossible
           Just TYPE_MESSAGE -> {- impossible -} iFail $ "Last entry was a TYPE_MESSAGE instead of concrete value type" -- impossible
           Just typeCode -> return typeCode
    -- Need to define a polymorphic 'done' to convert actual data type to its wire encoding
    let done :: Wire v => v -> RE (FieldId,ExtFieldValue)
        done v = let ft = FieldType (fromEnum t)
                     wt = toWireType ft
                     fid = fNumber fk
                 in return (fid,ExtFromWire (Seq.singleton (EP wt (runPut (wirePut ft v)))))
    -- The actual type and value fed to 'done' depends on the values 't' and 'uno':
    case t of
      TYPE_ENUM -> -- Now must also also handle Message and Group
        case (mVal fk,D.UninterpretedOption.identifier_value uno,D.UninterpretedOption.aggregate_value uno) of
          (Just (Right (E'Enum {eVals=enumVals})),Just enumVal,_) ->
            case validI enumVal of
              Nothing -> iFail $ "invalid D.UninterpretedOption.identifier_value: "++show enumVal
              Just enumIVal -> case M.lookup enumIVal enumVals of
                                 Nothing -> iFail $ "enumVal lookup failed: "++show (enumIVal,M.keys enumVals)
                                 Just val -> done (fromEnum val) -- fromEnum :: Int32 -> Int
          (Just (Right (E'Enum {})),Nothing,_) -> iFail $ "No identifer_value value to lookup in E'Enum"
          (Just (Right (E'Message {})),_,Nothing) -> iFail "Expected aggregate syntax to set a message option"
          (Just (Right (E'Message {})),_,Just aggVal) -> iFail $ "\n\n\
          \=========================================================================================\n\
          \Google's 2.4.0 aggregate syntax for message options is not yet supported, value would be:\n\
          \=========================================================================================\n" ++ show aggVal
          (Just (Right (E'Group {})),_,Nothing) -> iFail "Expected aggregate syntax to set a group option (impossible?)"
          (Just (Right (E'Group {})),_,Just aggVal) -> iFail $ "\n\n\
          \=========================================================================================\n\
          \Google's 2.4.0 aggregate syntax for message options is not yet supported, value would be:\n\
          \=========================================================================================\n" ++ show aggVal
          (me,_,_) -> iFail $ "Expected Just E'Enum or E'Message or E'Group, got:\n"++show me

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
  bVal = let true = Utf8 (U.fromString "true")
             false = Utf8 (U.fromString "false")
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


-- | Given a path, tries to find and parse a FileDescriptorProto
-- corresponding to it; returns also a canonicalised path.
type DescriptorReader m = (Monad m) => LocalFP -> m (D.FileDescriptorProto, LocalFP)

loadProto' :: (Functor r,Monad r) => DescriptorReader r -> LocalFP -> r (Env,[D.FileDescriptorProto])
loadProto' fdpReader protoFile = goState (load Set.empty protoFile) where
  goState act = do (env,m) <- runStateT act mempty
                   let fromRight (Right x) = x
                       fromRight (Left s) = error $ "loadProto failed to resolve a FileDescriptorProto: "++s
                   return (env,map (fromRight . top'FDP . fst . getTLS) (M.elems m))
  load parentsIn file = do
    built <- get
    when (Set.member file parentsIn)
         (loadFailed file (unlines ["imports failed: recursive loop detected"
                                   ,unlines . map show . M.assocs $ built,show parentsIn]))
    case M.lookup file built of  -- check memorized results
      Just result -> return result
      Nothing -> do
            (parsed'fdp, canonicalFile) <- lift $ fdpReader file
            let rawPackage = getPackage parsed'fdp
            packageName <- either (loadFailed canonicalFile . show)
                                  (return . fmap (map iToString . snd)) -- 2012-09-19 suspicious
                                  (checkPackageID rawPackage)

{-
-- OLD before 2012-09-19
            packageName <- either (loadFailed canonicalFile . show)
                                  (return . PackageID . map iToString . snd) -- 2012-09-19 suspicious
                                  (checkPackageID rawPackage)
-}

{-
   -- previously patched solution
            packageName <- case D.FileDescriptorProto.package parsed'fdp of
                             Nothing -> return []
                             Just p  -> either (loadFailed canonicalFile . show)
                                               (return . map iToString . snd) $
                                               (checkDIUtf8 p)
-}
            let parents = Set.insert file parentsIn
                importList = map (fpCanonToLocal . CanonFP . toString) . F.toList . D.FileDescriptorProto.dependency $ parsed'fdp
            imports <- mapM (fmap getTL . load parents) importList
            let eEnv = makeTopLevel parsed'fdp packageName imports -- makeTopLevel is the "internal entry point" of Resolve.hs
            -- Stricly force these two value to report errors here
            global'env <- either (loadFailed file) return eEnv
            _ <- either (loadFailed file) return (top'FDP . getTL $ global'env)
            modify (M.insert file global'env) -- add to memorized results
            return global'env

loadFailed :: (Monad m) => LocalFP -> String -> m a
loadFailed f msg = fail . unlines $ ["Parsing proto:",show (unLocalFP f),"has failed with message",msg]

-- | Given a list of paths to search, loads proto files by
-- looking for them in the file system.
loadProto :: [LocalFP] -> LocalFP -> IO (Env,[D.FileDescriptorProto])
loadProto protoDirs protoFile = loadProto' findAndParseSource protoFile where
      findAndParseSource :: DescriptorReader IO
      findAndParseSource file = do
        mayToRead <- liftIO $ findFile protoDirs file
        case mayToRead of
          Nothing -> loadFailed file (unlines (["loading failed, could not find file: "++show (unLocalFP file)
                                               ,"Searched paths were:"] ++ map (("  "++).show.unLocalFP) protoDirs))
          Just (toRead,relpath) -> do
            protoContents <- liftIO $ do putStrLn ("Loading filepath: "++show (unLocalFP toRead))
                                         LC.readFile (unLocalFP toRead)
            parsed'fdp <- either (loadFailed toRead . show) return $
                          (parseProto (unCanonFP relpath) protoContents)
            return (parsed'fdp, toRead)

loadCodeGenRequest :: CGR.CodeGeneratorRequest -> LocalFP -> (Env,[D.FileDescriptorProto])
loadCodeGenRequest req protoFile = runIdentity $ loadProto' lookUpParsedSource protoFile where
  lookUpParsedSource :: DescriptorReader Identity
  lookUpParsedSource file = case M.lookup file fdpsByName of
    Just result -> return (result, file)
    Nothing -> loadFailed file ("Request refers to file: "++show (unLocalFP file)
                                    ++" but it was not supplied in the request.")
  fdpsByName = M.fromList . map keyByName . F.toList . CGR.proto_file $ req
  keyByName fdp = (fdpName fdp, fdp)
  fdpName = LocalFP . maybe "" (LC.unpack . utf8) . D.FileDescriptorProto.name

-- wart: descend should take (eName,eMvals) not Entity
-- wart: myFix* obviously implements a WriterT by hand.  Implement as WriterT ?
