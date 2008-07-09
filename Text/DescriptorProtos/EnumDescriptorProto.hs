module Text.DescriptorProtos.EnumDescriptorProto
  (EnumDescriptorProto(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.EnumValueDescriptorProto as DescriptorProtos(EnumValueDescriptorProto)
import qualified Text.DescriptorProtos.EnumOptions as DescriptorProtos(EnumOptions)

data EnumDescriptorProto = EnumDescriptorProto
    { name :: Optional ByteString
    , value :: Seq DescriptorProtos.EnumValueDescriptorProto
    , options :: Optional DescriptorProtos.EnumOptions
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''EnumDescriptorProto )

instance OptionFlag a => Monoid (Option a EnumDescriptorProto) where mempty = Absent; mappend = op'Merge

instance Default EnumDescriptorProto where
