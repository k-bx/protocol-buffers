module Text.DescriptorProtos.MethodDescriptorProto
  (MethodDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.MethodOptions as DescriptorProtos(MethodOptions)

data MethodDescriptorProto = MethodDescriptorProto
    { name :: Maybe ByteString
    , input_type :: Maybe ByteString
    , output_type :: Maybe ByteString
    , options :: Maybe DescriptorProtos.MethodOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''MethodDescriptorProto )

instance Default MethodDescriptorProto where
