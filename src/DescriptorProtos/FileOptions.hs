module DescriptorProtos.FileOptions
  (FileOptions(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.FileOptions.OptimizeMode as DescriptorProtos.FileOptions(OptimizeMode)

data FileOptions = FileOptions
    { java_package :: Maybe String
    , java_outer_classname :: Maybe String
    , java_multiple_files :: Maybe Bool
    , optimize_for :: Maybe DescriptorProtos.FileOptions.OptimizeMode
    }
