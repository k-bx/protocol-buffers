module Text.DescriptorProtos.DescriptorProto.ExtensionRange
  (ExtensionRange(..))
 where

import Text.ProtocolBuffers.Header

data ExtensionRange = ExtensionRange
    { start :: Optional Int32
    , end :: Optional Int32
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''ExtensionRange )

instance OptionFlag a => Monoid (Option a ExtensionRange) where mempty = Absent; mappend = op'Merge

instance Default ExtensionRange where
