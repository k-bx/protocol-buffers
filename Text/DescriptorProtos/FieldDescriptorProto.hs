module Text.DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as DescriptorProtos.FieldDescriptorProto(Label)
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as DescriptorProtos.FieldDescriptorProto(Type)
import qualified Text.DescriptorProtos.FieldOptions as DescriptorProtos(FieldOptions)

data FieldDescriptorProto = FieldDescriptorProto
    { name :: Maybe ByteString
    , number :: Maybe Int32
    , labelEnum :: Maybe DescriptorProtos.FieldDescriptorProto.Label
    , typeEnum :: Maybe DescriptorProtos.FieldDescriptorProto.Type
    , type_name :: Maybe ByteString
    , extendee :: Maybe ByteString
    , default_value :: Maybe ByteString
    , options :: Maybe DescriptorProtos.FieldOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''FieldDescriptorProto )

instance Default FieldDescriptorProto where
