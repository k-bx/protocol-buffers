module Text.DescriptorProtos.EnumOptions
  (EnumOptions(..))
 where

import Text.ProtocolBuffers.Header

data EnumOptions = EnumOptions
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''EnumOptions )

instance Default EnumOptions where
