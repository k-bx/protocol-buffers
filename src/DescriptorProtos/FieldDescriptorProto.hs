module DescriptorProtos.FieldDescriptorProto
  (FieldDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.FieldDescriptorProto.Label as DescriptorProtos.FieldDescriptorProto(Label)
import qualified DescriptorProtos.FieldDescriptorProto.Type as DescriptorProtos.FieldDescriptorProto(Type)
import qualified DescriptorProtos.FieldOptions as DescriptorProtos(FieldOptions)

data FieldDescriptorProto = FieldDescriptorProto
    { name :: Optional ByteString
    , number :: Optional Int32
    , labelEnum :: Optional DescriptorProtos.FieldDescriptorProto.Label
    , typeEnum :: Optional DescriptorProtos.FieldDescriptorProto.Type
    , type_name :: Optional ByteString
    , extendee :: Optional ByteString
    , default_value :: Optional ByteString
    , options :: Optional DescriptorProtos.FieldOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''FieldDescriptorProto )

instance OptionFlag a => Monoid (Option a FieldDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default FieldDescriptorProto where
