module Text.DescriptorProtos.FileOptions
  (FileOptions(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions(OptimizeMode)
import qualified Text.DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions.OptimizeMode(OptimizeMode(..))

data FileOptions = FileOptions
    { java_package :: Maybe ByteString
    , java_outer_classname :: Maybe ByteString
    , java_multiple_files :: Maybe Bool
    , optimize_for :: Maybe DescriptorProtos.FileOptions.OptimizeMode
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''FileOptions )

instance Default FileOptions where
  defaultValue = mergeEmpty
    { java_multiple_files = Just False
    , optimize_for = Just DescriptorProtos.FileOptions.OptimizeMode.CODE_SIZE
    }
