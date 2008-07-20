module Text.DescriptorProtos.FieldOptions.CType
  (CType(..))
 where

import Text.ProtocolBuffers.Header

data CType = CORD | STRING_PIECE
  deriving (Show,Read,Eq,Ord,Data,Typeable)

$( makeMergeableEnum ''CType )

instance Default CType


