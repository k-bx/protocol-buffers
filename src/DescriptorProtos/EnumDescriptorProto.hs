module DescriptorProtos.EnumDescriptorProto
  (EnumDescriptorProto(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.EnumValueDescriptorProto as DescriptorProtos(EnumValueDescriptorProto)
import qualified DescriptorProtos.EnumOptions as DescriptorProtos(EnumOptions)

data EnumDescriptorProto = EnumDescriptorProto
    { name :: Optional ByteString
    , value :: Seq DescriptorProtos.EnumValueDescriptorProto
    , options :: Optional DescriptorProtos.EnumOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''EnumDescriptorProto )

instance OptionFlag a => Monoid (Option a EnumDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default EnumDescriptorProto where
