module DescriptorProtos.EnumDescriptorProto
  (EnumDescriptorProto(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.EnumValueDescriptorProto as DescriptorProtos(EnumValueDescriptorProto)
import qualified DescriptorProtos.EnumOptions as DescriptorProtos(EnumOptions)

data EnumDescriptorProto = EnumDescriptorProto
    { name :: Maybe String
    , value :: Seq DescriptorProtos.EnumValueDescriptorProto
    , options :: Maybe DescriptorProtos.EnumOptions
    }
