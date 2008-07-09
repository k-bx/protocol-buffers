module DescriptorProtos.EnumOptions
  (EnumOptions(..))
 where

import ProtocolBuffers.Header

data EnumOptions = EnumOptions
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''EnumOptions )

instance OptionFlag a => Monoid (Option a EnumOptions) where mempty = Absent; mappend = op'Merge

instance Default EnumOptions where
