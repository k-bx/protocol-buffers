module Text.DescriptorProtos.FieldDescriptorProto.Label
  (Label(..))
 where

import Text.ProtocolBuffers.Header

data Label = LABEL_OPTIONAL
           | LABEL_REQUIRED
           | LABEL_REPEATED
  deriving (Show,Eq,Ord,Typeable)

instance OptionFlag a => Monoid (Option a Label) where
  mempty = Absent
  mappend = flip const


