module Text.DescriptorProtos.ServiceDescriptorProto
  (ServiceDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.MethodDescriptorProto as DescriptorProtos(MethodDescriptorProto)
import qualified Text.DescriptorProtos.ServiceOptions as DescriptorProtos(ServiceOptions)

data ServiceDescriptorProto = ServiceDescriptorProto
    { name :: Maybe ByteString
    , method :: Seq DescriptorProtos.MethodDescriptorProto
    , options :: Maybe DescriptorProtos.ServiceOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''ServiceDescriptorProto )

instance Default ServiceDescriptorProto where
