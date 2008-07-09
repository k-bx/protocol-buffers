module DescriptorProtos.EnumValueOptions
  (EnumValueOptions(..))
 where

import ProtocolBuffers.Header

data EnumValueOptions = EnumValueOptions
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''EnumValueOptions )

instance OptionFlag a => Monoid (Option a EnumValueOptions) where mempty = Absent; mappend = op'Merge

instance Default EnumValueOptions where
