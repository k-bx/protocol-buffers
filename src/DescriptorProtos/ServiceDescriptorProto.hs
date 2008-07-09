module DescriptorProtos.ServiceDescriptorProto
  (ServiceDescriptorProto(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.MethodDescriptorProto as DescriptorProtos(MethodDescriptorProto)
import qualified DescriptorProtos.ServiceOptions as DescriptorProtos(ServiceOptions)

data ServiceDescriptorProto = ServiceDescriptorProto
    { name :: Optional ByteString
    , method :: Seq DescriptorProtos.MethodDescriptorProto
    , options :: Optional DescriptorProtos.ServiceOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''ServiceDescriptorProto )

instance OptionFlag a => Monoid (Option a ServiceDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default ServiceDescriptorProto where
