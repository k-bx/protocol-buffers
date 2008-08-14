module Text.DescriptorProtos.DescriptorProto.ExtensionRange
  (ExtensionRange(..))
 where

import Text.ProtocolBuffers.Header

data ExtensionRange = ExtensionRange
    { start :: Maybe Int32
    , end :: Maybe Int32
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''ExtensionRange )

instance Default ExtensionRange where
