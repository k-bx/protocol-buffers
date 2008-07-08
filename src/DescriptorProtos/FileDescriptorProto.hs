module DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.DescriptorProto as DescriptorProtos(DescriptorProto) 
import qualified DescriptorProtos.EnumDescriptorProto as DescriptorProtos(EnumDescriptorProto) 
import qualified DescriptorProtos.FieldDescriptorProto as DescriptorProtos(FieldDescriptorProto) 
import qualified DescriptorProtos.FileOptions as DescriptorProtos(FileOptions)
import qualified DescriptorProtos.ServiceDescriptorProto as DescriptorProtos(ServiceDescriptorProto) 

data FileDescriptorProto = FileDescriptorProto
    { name :: Maybe String
    , package :: Maybe String
    , dependency :: Seq String
    , message_type :: Seq DescriptorProtos.DescriptorProto
    , enum_type :: Seq DescriptorProtos.EnumDescriptorProto
    , service :: Seq DescriptorProtos.ServiceDescriptorProto
    , extension :: Seq DescriptorProtos.FieldDescriptorProto
    , options :: Maybe DescriptorProtos.FileOptions
    }
