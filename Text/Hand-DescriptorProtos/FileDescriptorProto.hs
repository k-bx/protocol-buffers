module Text.DescriptorProtos.FileDescriptorProto
  (FileDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.DescriptorProto as DescriptorProtos(DescriptorProto) 
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.FileOptions as DescriptorProtos(FileOptions)
import qualified Text.DescriptorProtos.ServiceDescriptorProto as DescriptorProtos(ServiceDescriptorProto) 

data FileDescriptorProto = FileDescriptorProto
    { name :: Maybe ByteString
    , package :: Maybe ByteString
    , dependency :: Seq ByteString
    , message_type :: Seq DescriptorProtos.DescriptorProto
    , enum_type :: Seq DescriptorProtos.EnumDescriptorProto
    , service :: Seq DescriptorProtos.ServiceDescriptorProto
    , extension :: Seq DescriptorProtos.FieldDescriptorProto
    , options :: Maybe DescriptorProtos.FileOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''FileDescriptorProto )

instance Default FileDescriptorProto where
