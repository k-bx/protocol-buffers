module DescriptorProtos.MethodDescriptorProto
  (MethodDescriptorProto(..))
 where

import ProtocolBuffers.Header
import qualified DescriptorProtos.MethodOptions as DescriptorProtos(MethodOptions)

data MethodDescriptorProto = MethodDescriptorProto
    { name :: Optional ByteString
    , input_type :: Optional ByteString
    , output_type :: Optional ByteString
    , options :: Optional DescriptorProtos.MethodOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''MethodDescriptorProto )

instance OptionFlag a => Monoid (Option a MethodDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default MethodDescriptorProto where
