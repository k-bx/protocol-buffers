module Text.DescriptorProtos.DescriptorProto
  (DescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.FileOptions as DescriptorProtos(FileOptions)
import qualified Text.DescriptorProtos.EnumDescriptorProto as DescriptorProtos(EnumDescriptorProto) 
import qualified Text.DescriptorProtos.FieldDescriptorProto as DescriptorProtos(FieldDescriptorProto) 
import qualified Text.DescriptorProtos.MessageOptions as DescriptorProtos(MessageOptions)
import qualified Text.DescriptorProtos.DescriptorProto.ExtensionRange as DescriptorProtos.DescriptorProto(ExtensionRange)

data DescriptorProto = DescriptorProto
    { name :: Maybe ByteString
    , field :: Seq DescriptorProtos.FieldDescriptorProto
    , extension :: Seq DescriptorProtos.FieldDescriptorProto
    , nested_type :: Seq DescriptorProto
    , enum_type :: Seq DescriptorProtos.EnumDescriptorProto
    , extension_range :: Seq DescriptorProtos.DescriptorProto.ExtensionRange
    , options :: Maybe DescriptorProtos.MessageOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''DescriptorProto )

instance Default DescriptorProto where
