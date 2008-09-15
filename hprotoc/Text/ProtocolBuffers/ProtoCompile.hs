-- | This is the Main module for the command line program
module Main where

import qualified Data.Map as M
import Data.Version
import Language.Haskell.Pretty(prettyPrintStyleMode,Style(..),Mode(..),PPHsMode(..),PPLayout(..))
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath

import Text.ProtocolBuffers.Reflections(ProtoInfo(..),DescriptorInfo(..),EnumInfo(..))

import Text.ProtocolBuffers.ProtoCompile.Gen(protoModule,descriptorModule,enumModule)
import Text.ProtocolBuffers.ProtoCompile.Resolve(loadProto)
import Text.ProtocolBuffers.ProtoCompile.MakeReflections(makeProtoInfo,serializeFDP)

-- | Version of protocol-buffers.
-- The version tags that I have used are ["unreleased"]
version :: Version
version = Version { versionBranch = [0,2,6]
                  , versionTags = ["unreleased"] }

data Options = Options { optPrefix :: String
                       , optTarget :: FilePath
                       , optInclude :: [FilePath]
                       , optProto :: FilePath
                       , optVerbose :: Bool
                       , optUnkownFields :: Bool }
  deriving Show

setPrefix,setTarget,setInclude,setProto :: String -> Options -> Options
setVerbose,setUnknown :: Options -> Options
setPrefix   s o = o { optPrefix = s }
setTarget   s o = o { optTarget = s }
setInclude  s o = o { optInclude = s : optInclude o }
setProto    s o = o { optProto = s }
setVerbose    o = o { optVerbose = True }
setUnknown    o = o { optUnkownFields = True }

data OptionAction = Mutate (Options->Options) | Run (Options->Options) | Switch Flag

data Flag = VersionInfo

optionList :: [OptDescr OptionAction]
optionList =
  [ Option ['I'] ["proto_path"] (ReqArg (Mutate . setInclude) "DIR")
               "directory from which to search for imported proto files (default is pwd); all DIR searched"
  , Option ['o'] ["haskell_out"] (ReqArg (Mutate . setTarget) "DIR")
               "directory to use are root of generated files (default is pwd); last flag"
  , Option ['p'] ["prefix"] (ReqArg (Mutate . setPrefix) "MODULE")
               "dotted haskell MODULE name to use as prefix (default is none); last flag used"
  , Option ['u'] ["unknown-fields"] (NoArg (Mutate setUnknown))
               "UNIMPLEMENTED: generated messages and groups all support unknown fields"
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
  pwd <- getCurrentDirectory
  return $ Options { optPrefix = "", optTarget = pwd, optInclude = [pwd], optProto = "", optVerbose = False, optUnkownFields = False }

main :: IO ()
main = do
  defs <- defaultOptions
  args <- getArgs
  case processOptions args of
    Left msg -> putStrLn msg
    Right todo -> process defs todo

process :: Options -> [OptionAction] -> IO ()
process options [] = if null (optProto options)
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
style = Style PageMode 132 0.5

myMode :: PPHsMode
myMode = PPHsMode 2 2 2 2 4 2 True PPOffsideRule False True

run :: Options -> IO ()
run options = do
  print options
  protos <- loadProto (optInclude options) (optProto options)
  let (Just (fdp,_,names)) = M.lookup (optProto options) protos
      protoInfo = makeProtoInfo (optPrefix options) names fdp
  let produceMSG di = do
        let file = combine (optTarget options) . joinPath . descFilePath $ di
        print file
        mkdirFor file
        writeFile file (prettyPrintStyleMode style myMode (descriptorModule di))
      produceENM ei = do
        let file = combine (optTarget options) . joinPath . enumFilePath $ ei
        print file
        mkdirFor file
        writeFile file (prettyPrintStyleMode style myMode (enumModule ei))
  mapM_ produceMSG (messages protoInfo)
  mapM_ produceENM (enums protoInfo)

  let file = combine (optTarget options) . joinPath . protoFilePath $ protoInfo
  print file
  writeFile file (prettyPrintStyleMode style myMode (protoModule protoInfo (serializeFDP fdp)))

