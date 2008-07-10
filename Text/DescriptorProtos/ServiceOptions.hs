module Text.DescriptorProtos.ServiceOptions
  (ServiceOptions(..))
 where

import Text.ProtocolBuffers.Header

data ServiceOptions = ServiceOptions
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''ServiceOptions )

instance Default ServiceOptions where
