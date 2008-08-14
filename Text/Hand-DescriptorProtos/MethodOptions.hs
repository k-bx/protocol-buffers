module Text.DescriptorProtos.MethodOptions
  (MethodOptions(..))
 where

import Text.ProtocolBuffers.Header

data MethodOptions = MethodOptions
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''MethodOptions )

instance Default MethodOptions where
