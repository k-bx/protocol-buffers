module Text.DescriptorProtos.EnumValueDescriptorProto
  (EnumValueDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.EnumValueOptions as DescriptorProtos(EnumValueOptions) 

data EnumValueDescriptorProto = EnumValueDescriptorProto
    { name :: Optional ByteString
    , number :: Optional Int32
    , options :: Optional DescriptorProtos.EnumValueOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''EnumValueDescriptorProto )

instance OptionFlag a => Monoid (Option a EnumValueDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default EnumValueDescriptorProto where
