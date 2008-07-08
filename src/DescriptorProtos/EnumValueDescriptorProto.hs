module DescriptorProtos.EnumValueDescriptorProto
  (EnumValueDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.EnumValueOptions as DescriptorProtos(EnumValueOptions) 

data EnumValueDescriptorProto = EnumValueDescriptorProto
    { name :: Maybe String
    , number :: Maybe Int32
    , options :: Maybe DescriptorProtos.EnumValueOptions
    }
