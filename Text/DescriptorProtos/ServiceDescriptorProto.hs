module Text.DescriptorProtos.ServiceDescriptorProto
  (ServiceDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.MethodDescriptorProto as DescriptorProtos(MethodDescriptorProto)
import qualified Text.DescriptorProtos.ServiceOptions as DescriptorProtos(ServiceOptions)

data ServiceDescriptorProto = ServiceDescriptorProto
    { name :: Optional ByteString
    , method :: Seq DescriptorProtos.MethodDescriptorProto
    , options :: Optional DescriptorProtos.ServiceOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''ServiceDescriptorProto )

instance OptionFlag a => Monoid (Option a ServiceDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default ServiceDescriptorProto where
