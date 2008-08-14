module Text.DescriptorProtos.MessageOptions
  (MessageOptions(..))
 where

import Text.ProtocolBuffers.Header

data MessageOptions = MessageOptions
    { message_set_wire_format :: Maybe Bool
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''MessageOptions )

instance Default MessageOptions where
  defaultValue = mergeEmpty
    { message_set_wire_format = Just False
    }
