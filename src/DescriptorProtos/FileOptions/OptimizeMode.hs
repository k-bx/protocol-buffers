module DescriptorProtos.FileOptions.OptimizeMode
  (OptimizeMode(..))
 where

import ProtocolBuffers.Header

data OptimizeMode = SPEED | CODE_SIZE
  deriving (Show,Eq,Ord,Typeable)


