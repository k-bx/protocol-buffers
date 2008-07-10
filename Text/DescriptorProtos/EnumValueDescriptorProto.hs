module Text.DescriptorProtos.EnumValueDescriptorProto
  (EnumValueDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.EnumValueOptions as DescriptorProtos(EnumValueOptions) 

data EnumValueDescriptorProto = EnumValueDescriptorProto
    { name :: Maybe ByteString
    , number :: Maybe Int32
    , options :: Maybe DescriptorProtos.EnumValueOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''EnumValueDescriptorProto )

instance Default EnumValueDescriptorProto where
