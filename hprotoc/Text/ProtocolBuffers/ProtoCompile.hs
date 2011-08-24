-- | This is the Main module for the command line program 'hprotoc'
module Main where

import Control.Monad(unless,forM_)
import Control.Monad.State(State, execState, modify)
import qualified Data.ByteString.Lazy.Char8 as LC (hGetContents, hPut, pack, unpack)
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (fromList,singleton)
import Data.Sequence ((|>))
import Data.Version(showVersion)
import Language.Haskell.Exts.Pretty(prettyPrintStyleMode,Style(..),Mode(..),PPHsMode(..),PPLayout(..))
import System.Console.GetOpt(OptDescr(Option),ArgDescr(NoArg,ReqArg)
                            ,usageInfo,getOpt,ArgOrder(ReturnInOrder))
import System.Directory(getCurrentDirectory,createDirectoryIfMissing)
import System.Environment(getProgName, getArgs)
import System.FilePath(takeDirectory,combine,joinPath)
import qualified System.FilePath.Posix as Canon(takeBaseName)
import System.IO (stdin, stdout)

import Text.ProtocolBuffers.Basic(defaultValue, Utf8(..), utf8)
import Text.ProtocolBuffers.Identifiers(MName,checkDIString,mangle)
import Text.ProtocolBuffers.Reflections(ProtoInfo(..),EnumInfo(..))
import Text.ProtocolBuffers.WireMessage (messagePut, messageGet)

import qualified Text.DescriptorProtos.FileDescriptorProto as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto as D.FileDescriptorProto(FileDescriptorProto(..))
-- import qualified Text.DescriptorProtos.FileDescriptorSet   as D(FileDescriptorSet)
import qualified Text.DescriptorProtos.FileDescriptorSet   as D.FileDescriptorSet(FileDescriptorSet(..))

import Text.ProtocolBuffers.ProtoCompile.BreakRecursion(makeResult)
import Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModules,enumModule)
import Text.ProtocolBuffers.ProtoCompile.MakeReflections(makeProtoInfo,serializeFDP)
import Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto,loadCodeGenRequest,makeNameMaps,getTLS
                                                ,Env,LocalFP(..),CanonFP(..),TopLevel(..))

import Text.Google.Protobuf.Compiler.CodeGeneratorRequest
import Text.Google.Protobuf.Compiler.CodeGeneratorResponse hiding (error, file)
import qualified Text.Google.Protobuf.Compiler.CodeGeneratorResponse as CGR(file)
import qualified Text.Google.Protobuf.Compiler.CodeGeneratorResponse.File as CGR.File

-- The Paths_hprotoc module is produced by cabal
import Paths_hprotoc(version)

data Options = Options { optPrefix :: [MName String]
                       , optAs :: [(CanonFP,[MName String])]
                       , optTarget :: LocalFP
                       , optInclude :: [LocalFP]
                       , optProto :: LocalFP
                       , optDesc :: Maybe (LocalFP)
                       , optImports,optVerbose,optUnknownFields,optLazy,optDryRun :: Bool }
  deriving Show

setPrefix,setTarget,setInclude,setProto,setDesc :: String -> Options -> Options
setImports,setVerbose,setUnknown,setLazy,setDryRun :: Options -> Options
setPrefix   s o = o { optPrefix = toPrefix s }
setTarget   s o = o { optTarget = (LocalFP s) }
setInclude  s o = o { optInclude = LocalFP s : optInclude o }
setProto    s o = o { optProto = LocalFP s }
setDesc     s o = o { optDesc = Just (LocalFP s) }
setImports    o = o { optImports = True }
setVerbose    o = o { optVerbose = True }
setUnknown    o = o { optUnknownFields = True }
setLazy       o = o { optLazy = True }
setDryRun     o = o { optDryRun = True }

toPrefix :: String -> [MName String]
toPrefix s = case checkDIString s of
               Left msg -> error $ "Bad module name in options:"++show s++"\n"++msg
               Right (True,_) -> error $ "Bad module name in options (cannot start with '.'): "++show s
               Right (False,ms) -> map mangle ms

-- | 'setAs' puts both the full path and the basename as keys into the association list
setAs :: String -> Options -> Options
setAs s o =
   case break ('='==) s of
     (filepath,'=':rawPrefix) -> let value = toPrefix rawPrefix
                                 in o { optAs = (CanonFP filepath,value):
                                                (CanonFP (Canon.takeBaseName filepath),value):
                                                optAs o}
     _ -> error . unlines $ [ "Malformed -a or --as option "++show s
                            , "  Expected \"FILEPATH=MODULE\""
                            , "  where FILEPATH is the basename or relative path (using '/') of an imported file"
                            , "  where MODULE is a dotted Haskell name to use as a prefix"
                            , "  also MODULE can be empty to have no prefix" ]

data OptionAction = Mutate (Options->Options) | Run (Options->Options) | Switch Flag

data Flag = VersionInfo

optionList :: [OptDescr OptionAction]
optionList =
  [ Option ['a'] ["as"] (ReqArg (Mutate . setAs) "FILEPATH=MODULE")
               "assign prefix module to imported proto file: --as descriptor.proto=Text"
  , Option ['I'] ["proto_path"] (ReqArg (Mutate . setInclude) "DIR")
               "directory from which to search for imported proto files (default is pwd); all DIR searched"
  , Option ['d'] ["haskell_out"] (ReqArg (Mutate . setTarget) "DIR")
               "directory to use are root of generated files (default is pwd); last flag"
  , Option ['n'] ["dry_run"] (NoArg (Mutate setDryRun))
               "produce no output but still parse and check the proto file(s)"
  , Option ['o'] ["descriptor_set_out"] (ReqArg (Mutate . setDesc) "FILE")
               "filename to write binary FileDescriptorSet to"
  , Option [] ["include_imports"] (NoArg (Mutate setImports))
               "when writing descriptor_set_out include all imported files to be self-contained"
  , Option ['p'] ["prefix"] (ReqArg (Mutate . setPrefix) "MODULE")
               "dotted Haskell MODULE name to use as a prefix (default is none); last flag used"
  , Option ['u'] ["unknown_fields"] (NoArg (Mutate setUnknown))
               "generated messages and groups all support unknown fields"
  , Option ['l'] ["lazy_fields"] (NoArg (Mutate setLazy))
               "new default is now messages with strict fields, this reverts to generating lazy fields"
  , Option ['v'] ["verbose"] (NoArg (Mutate  setVerbose))
               "increase amount of printed information"
  , Option [] ["version"]  (NoArg (Switch VersionInfo))
               "print out version information"
  ]

usageMsg,versionInfo :: String
usageMsg = usageInfo "Usage: protoCompile [OPTION..] path-to-file.proto ..." optionList

versionInfo = unlines $
  [ "Welcome to protocol-buffers version "++showVersion version
  , "Copyright (c) 2008-2011, Christopher Kuklewicz."
  , "Released under BSD3 style license, see LICENSE file for details."
  , "Some proto files, such as descriptor.proto and unittest*.proto"
  , "are from google's code and are under an Apache 2.0 license."
  , ""
  , "Most command line arguments are similar to those of 'protoc'."
  , ""
  , "This program reads a .proto file and generates haskell code files."
  , "See http://code.google.com/apis/protocolbuffers/docs/overview.html for more."
  ]

processOptions :: [String] -> Either String [OptionAction]
processOptions argv =
    case getOpt (ReturnInOrder (Run . setProto)) optionList argv of
    (opts,_,[]) -> Right opts
    (_,_,errs) -> Left (unlines errs ++ usageMsg)

defaultOptions :: IO Options
defaultOptions = do
  pwd <- fmap LocalFP getCurrentDirectory
  return $ Options { optPrefix = []
                   , optAs = []
                   , optTarget = pwd
                   , optInclude = [pwd]
                   , optProto = LocalFP ""
                   , optDesc = Nothing
                   , optImports = False
                   , optVerbose = False
                   , optUnknownFields = False
                   , optLazy = False
                   , optDryRun = False }

main :: IO ()
main = do
  progName <- getProgName
  case progName of
    "protoc-gen-haskell" -> pluginMain
    _                    -> standaloneMain

pluginMain :: IO ()
pluginMain = do
  defs <- defaultOptions
  inputBytes <- LC.hGetContents stdin
  let req = either error fst $ messageGet inputBytes
  let prefix = fmap (LC.unpack . utf8) $ parameter req
  let opts = maybe defs (flip setPrefix $ defs) prefix
  let resp = runPlugin opts req
  LC.hPut stdout $ messagePut resp

standaloneMain :: IO ()
standaloneMain = do
  defs <- defaultOptions
  args <- getArgs
  case processOptions args of
    Left msg -> putStrLn msg
    Right todo -> process defs todo

process :: Options -> [OptionAction] -> IO ()
process options [] = if null (unLocalFP (optProto options))
                       then do putStrLn "No proto file specified (or empty proto file)"
                               putStrLn ""
                               putStrLn usageMsg
                       else putStrLn "Processing complete, have a nice day."
process options (Mutate f:rest) = process (f options) rest
process options (Run f:rest) = let options' = f options
                            in runStandalone options' >> process options' rest
process _options (Switch VersionInfo:_) = putStrLn versionInfo
  
mkdirFor :: FilePath -> IO ()
mkdirFor p = createDirectoryIfMissing True (takeDirectory p)

style :: Style
style = Style PageMode 132 0.6

myMode :: PPHsMode
myMode = PPHsMode 2 2 2 2 4 1 True PPOffsideRule False -- True

dump :: (Monad m) => Output m -> Bool -> Maybe LocalFP -> D.FileDescriptorProto -> [D.FileDescriptorProto] -> m ()
dump _ _ Nothing _ _ = return ()
dump o imports (Just (LocalFP dumpFile)) fdp fdps = do
  outputReport o $ "dumping to filename: "++show dumpFile
  let s = if imports then Seq.fromList fdps else Seq.singleton fdp
  outputWriteFile o dumpFile $ LC.unpack (messagePut $ defaultValue { D.FileDescriptorSet.file = s })
  outputReport o $ "finished dumping FileDescriptorSet binary of: "++show (D.FileDescriptorProto.name fdp)

data (Monad m) => Output m = Output {
  outputReport :: String -> m (),
  outputWriteFile :: FilePath -> String -> m ()
}

runStandalone :: Options -> IO ()
runStandalone options = do
  (env,fdps) <- loadProto (optInclude options) (optProto options)
  putStrLn "All proto files loaded"
  run' standaloneMode options env fdps where
    standaloneMode :: Output IO
    standaloneMode = Output putStrLn emitFile
    emitFile file contents = do
      let fullPath = combine (unLocalFP . optTarget $ options) file
      putStrLn fullPath
      mkdirFor fullPath
      unless (optDryRun options) $ writeFile fullPath contents

runPlugin :: Options -> CodeGeneratorRequest -> CodeGeneratorResponse
runPlugin options req = execState (run' pluginOutput options env fdps) defaultValue where
  (env,fdps) = loadCodeGenRequest req (head requestedFiles)
  pluginOutput :: Output (State CodeGeneratorResponse)
  pluginOutput = Output {
      outputReport = const $ return (),
      outputWriteFile = appendFileRecord
    }
  appendFileRecord :: FilePath -> String -> State CodeGeneratorResponse ()
  appendFileRecord f c = modify $ \resp -> resp {
          CGR.file = (CGR.file resp) |> newFile
        } where
      newFile = defaultValue {
          CGR.File.name = Just $ Utf8 $ LC.pack f,
          CGR.File.content = Just $ Utf8 $ LC.pack c
        }
  requestedFiles = map (LocalFP . LC.unpack . utf8) . toList . file_to_generate $ req

-- This run' operates for both runStandalone and runPlugin
run' :: (Monad m) => Output m -> Options -> Env -> [D.FileDescriptorProto] -> m ()
run' o@(Output print' writeFile') options env fdps = do
  let fdp = either error id . top'FDP . fst . getTLS $ env
  unless (optDryRun options) $ dump o (optImports options) (optDesc options) fdp fdps
  -- Compute the nameMap that determine how to translate from proto names to haskell names
  -- This is the part that uses the (optional) package name
  nameMap <- either error return $ makeNameMaps (optPrefix options) (optAs options) env
  print' "Haskell name mangling done"
  let protoInfo = makeProtoInfo (optUnknownFields options,optLazy options) nameMap fdp
      result = makeResult protoInfo
  seq result (print' "Recursive modules resolved")
  let produceMSG di = do
        unless (optDryRun options) $ do
          -- There might be several modules
          let fileModules = descriptorModules result di
          forM_ fileModules $ \ (relPath,modSyn) -> do
            writeFile' relPath (prettyPrintStyleMode style myMode modSyn)
      produceENM ei = do
        let file = joinPath . enumFilePath $ ei
        writeFile' file (prettyPrintStyleMode style myMode (enumModule ei))
  mapM_ produceMSG (messages protoInfo)
  mapM_ produceENM (enums protoInfo)
  let file = joinPath . protoFilePath $ protoInfo
  writeFile' file (prettyPrintStyleMode style myMode (protoModule result protoInfo (serializeFDP fdp)))
