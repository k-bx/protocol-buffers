module DescriptorProtos.ServiceDescriptorProto
  (ServiceDescriptorProto(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.MethodDescriptorProto as DescriptorProtos(MethodDescriptorProto)
import qualified DescriptorProtos.ServiceOptions as DescriptorProtos(ServiceOptions)

data ServiceDescriptorProto = ServiceDescriptorProto
    { name :: Maybe String
    , method :: Seq DescriptorProtos.MethodDescriptorProto
    , options :: Maybe DescriptorProtos.ServiceOptions
    }
