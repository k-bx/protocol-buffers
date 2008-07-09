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
    { name :: Optional ByteString
    , package :: Optional ByteString
    , dependency :: Seq ByteString
    , message_type :: Seq DescriptorProtos.DescriptorProto
    , enum_type :: Seq DescriptorProtos.EnumDescriptorProto
    , service :: Seq DescriptorProtos.ServiceDescriptorProto
    , extension :: Seq DescriptorProtos.FieldDescriptorProto
    , options :: Optional DescriptorProtos.FileOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''FileDescriptorProto )

instance OptionFlag a => Monoid (Option a FileDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default FileDescriptorProto where
