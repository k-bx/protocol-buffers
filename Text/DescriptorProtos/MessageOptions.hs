module Text.DescriptorProtos.MessageOptions
  (MessageOptions(..))
 where

import Text.ProtocolBuffers.Header

data MessageOptions = MessageOptions
    { message_set_wire_format :: Optional Bool
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''MessageOptions )

instance OptionFlag a => Monoid (Option a MessageOptions) where mempty = Absent; mappend = op'Merge

instance Default MessageOptions where
  defaultValue = mempty
    { message_set_wire_format = Present False
    }
