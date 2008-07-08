module DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.FieldDescriptorProto.Label as DescriptorProtos.FieldDescriptorProto(Label)
import qualified DescriptorProtos.FieldDescriptorProto.Type as DescriptorProtos.FieldDescriptorProto(Type)
import qualified DescriptorProtos.FieldOptions as DescriptorProtos(FieldOptions)

data FieldDescriptorProto = FieldDescriptorProto
    { name :: Maybe String
    , number :: Maybe Int32
    , labelEnum :: Maybe DescriptorProtos.FieldDescriptorProto.Label
    , typeEnum :: Maybe DescriptorProtos.FieldDescriptorProto.Type
    , type_name :: Maybe String
    , extendee :: Maybe String
    , default_value :: Maybe String
    , options :: Maybe DescriptorProtos.FieldOptions
    }
