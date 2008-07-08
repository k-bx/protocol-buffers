module DescriptorProtos.MethodDescriptorProto
  (MethodDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.MethodOptions as DescriptorProtos(MethodOptions)

data MethodDescriptorProto = MethodDescriptorProto
    { name :: Maybe String
    , input_type :: Maybe String
    , output_type :: Maybe String
    , options :: Maybe DescriptorProtos.MethodOptions
    }
