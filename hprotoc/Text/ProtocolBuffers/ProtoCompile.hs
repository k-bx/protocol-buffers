-- | This is the Main module for the command line program 'hprotoc'
module Main where

import Control.Monad(when)
import qualified Data.ByteString.Lazy.Char8 as LC (writeFile)
import Data.List(break)
import qualified Data.Sequence as Seq (fromList,singleton)
import Data.Version(Version(..),showVersion)
import Language.Haskell.Pretty(prettyPrintStyleMode,Style(..),Mode(..),PPHsMode(..),PPLayout(..))
import System.Console.GetOpt(OptDescr(Option),ArgDescr(NoArg,ReqArg)
                            ,usageInfo,getOpt,ArgOrder(ReturnInOrder))
import System.Directory(getCurrentDirectory,createDirectoryIfMissing)
import System.Environment(getArgs)
import System.FilePath(takeDirectory,combine,joinPath)
import qualified System.FilePath.Posix as Canon(takeBaseName)

import Text.ProtocolBuffers.Basic(defaultValue)
import Text.ProtocolBuffers.Identifiers(MName,checkDIString,mangle)
import Text.ProtocolBuffers.Reflections(ProtoInfo(..),DescriptorInfo(..),EnumInfo(..))
import Text.ProtocolBuffers.WireMessage (messagePut)

import qualified Text.DescriptorProtos.FileDescriptorProto as D(FileDescriptorProto)
import qualified Text.DescriptorProtos.FileDescriptorProto as D.FileDescriptorProto(FileDescriptorProto(..))
import qualified Text.DescriptorProtos.FileDescriptorSet   as D(FileDescriptorSet)
import qualified Text.DescriptorProtos.FileDescriptorSet   as D.FileDescriptorSet(FileDescriptorSet(..))

import Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModule,enumModule)
import Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto,makeNameMaps,getTLS
                                                ,LocalFP(..),CanonFP(..),TopLevel(..))
import Text.ProtocolBuffers.ProtoCompile.MakeReflections(makeProtoInfo,serializeFDP)

-- | Version of protocol-buffers.
-- The version tags that I have used are ["unreleased"]
version :: Version
version = Version { versionBranch = [1,2,1]
                  , versionTags = [] }

data Options = Options { optPrefix :: [MName String]
                       , optAs :: [(CanonFP,[MName String])]
                       , optTarget :: LocalFP
                       , optInclude :: [LocalFP]
                       , optProto :: LocalFP
                       , optDesc :: Maybe (LocalFP)
                       , optImports,optVerbose,optUnknownFields,optDryRun :: Bool }
  deriving Show

setPrefix,setTarget,setInclude,setProto,setDesc :: String -> Options -> Options
setImports,setVerbose,setUnknown,setDryRun :: Options -> Options
setPrefix   s o = o { optPrefix = toPrefix s }
setTarget   s o = o { optTarget = (LocalFP s) }
setInclude  s o = o { optInclude = LocalFP s : optInclude o }
setProto    s o = o { optProto = LocalFP s }
setDesc     s o = o { optDesc = Just (LocalFP s) }
setImports    o = o { optImports = True }
setVerbose    o = o { optVerbose = True }
setUnknown    o = o { optUnknownFields = True }
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
               "assign prefix module to imported prot file: --as decriptor.proto=Text"
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
  , Option ['v'] ["verbose"] (NoArg (Mutate  setVerbose))
               "increase amount of printed information"
  , Option [] ["version"]  (NoArg (Switch VersionInfo))
               "print out version information"
  ]

usageMsg,versionInfo :: String
usageMsg = usageInfo "Usage: protoCompile [OPTION..] path-to-file.proto ..." optionList

versionInfo = unlines $
  [ "Welcome to protocol-buffers version "++showVersion version
  , "Copyright (c) 2008, Christopher Kuklewicz."
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
                   , optDryRun = False }

main :: IO ()
main = do
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
                            in run options' >> process options' rest
process _options (Switch VersionInfo:_) = putStrLn versionInfo
  
mkdirFor :: FilePath -> IO ()
mkdirFor p = createDirectoryIfMissing True (takeDirectory p)

style :: Style
style = Style PageMode 132 0.6

myMode :: PPHsMode
myMode = PPHsMode 2 2 2 2 4 1 True PPOffsideRule False True

dump :: Bool -> Maybe LocalFP -> D.FileDescriptorProto -> [D.FileDescriptorProto] -> IO ()
dump _ Nothing _ _ = return ()
dump imports (Just (LocalFP dumpFile)) fdp fdps = do
  putStrLn $ "dumping to filename: "++show dumpFile
  let s = if imports then Seq.fromList fdps else Seq.singleton fdp
  LC.writeFile dumpFile (messagePut $ defaultValue { D.FileDescriptorSet.file = s })
  putStrLn $ "finished dumping FileDescriptorSet binary of: "++show (D.FileDescriptorProto.name fdp)

run :: Options -> IO ()
run options = do
  (env,fdps) <- loadProto (optInclude options) (optProto options)
  print "All proto files loaded"
  let fdp = either error id . top'FDP . fst . getTLS $ env
  when (not (optDryRun options)) $ dump (optImports options) (optDesc options) fdp fdps
  nameMap <- either error return $ makeNameMaps (optPrefix options) (optAs options) env
  print "Haskell name mangling done"
  let protoInfo = makeProtoInfo (optUnknownFields options) nameMap fdp
  let produceMSG di = do
        let file = combine (unLocalFP . optTarget $ options) . joinPath . descFilePath $ di
        print file
        when (not (optDryRun options)) $ do
          mkdirFor file
          writeFile file (prettyPrintStyleMode style myMode (descriptorModule di))
      produceENM ei = do
        let file = combine (unLocalFP . optTarget $ options) . joinPath . enumFilePath $ ei
        print file
        when (not (optDryRun options)) $ do
          mkdirFor file
          writeFile file (prettyPrintStyleMode style myMode (enumModule ei))
  mapM_ produceMSG (messages protoInfo)
  mapM_ produceENM (enums protoInfo)

  let file = combine (unLocalFP . optTarget $ options) . joinPath . protoFilePath $ protoInfo
  print file
  writeFile file (prettyPrintStyleMode style myMode (protoModule protoInfo (serializeFDP fdp)))
