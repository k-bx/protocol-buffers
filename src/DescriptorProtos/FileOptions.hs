module DescriptorProtos.FileOptions
  (FileOptions(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions(OptimizeMode)
import qualified DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions.OptimizeMode(OptimizeMode(..))

data FileOptions = FileOptions
    { java_package :: Optional ByteString
    , java_outer_classname :: Optional ByteString
    , java_multiple_files :: Optional Bool
    , optimize_for :: Optional DescriptorProtos.FileOptions.OptimizeMode
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''FileOptions )

instance OptionFlag a => Monoid (Option a FileOptions) where mempty = Absent; mappend = op'Merge

instance Default FileOptions where
  defaultValue = mempty
    { java_multiple_files = Present False
    , optimize_for = Present DescriptorProtos.FileOptions.OptimizeMode.CODE_SIZE
    }
