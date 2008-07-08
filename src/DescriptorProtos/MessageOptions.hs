module DescriptorProtos.MessageOptions
  (MessageOptions(..))
 where

import ProtocolBuffers.Header

data MessageOptions = MessageOptions
    { message_set_wire_format :: Maybe Bool
    }
