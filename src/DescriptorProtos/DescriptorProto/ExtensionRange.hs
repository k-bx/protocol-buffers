module DescriptorProtos.DescriptorProto.ExtensionRange
  (ExtensionRange(..))
 where

import ProtocolBuffers.Header

data ExtensionRange = ExtensionRange
    { start :: Int32
    , end :: Int32
    }
