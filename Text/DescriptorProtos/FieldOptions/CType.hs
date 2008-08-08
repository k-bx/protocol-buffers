module Text.DescriptorProtos.FieldOptions.CType
  (CType(..))
 where

import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'

data CType = CORD | STRING_PIECE
  deriving (P'.Show,P'.Read,P'.Eq,P'.Ord,P'.Data,P'.Typeable)

-- $( P'.makeMergeableEnum ''CType )

instance P'.Mergeable CType

instance P'.Default CType

instance P'.Wire CType

instance P'.ReflectEnum CType
