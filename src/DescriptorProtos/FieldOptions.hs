module DescriptorProtos.FieldOptions
  (FieldOptions(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions(CType)

data FieldOptions = FieldOptions
    { ctype :: Optional DescriptorProtos.FieldOptions.CType
    , experimental_map_key :: Optional ByteString
    }
  deriving (Show,Eq,Ord,Typeable)

$( derive makeMonoid ''FieldOptions )

instance OptionFlag a => Monoid (Option a FieldOptions) where mempty = Absent; mappend = op'Merge

instance Default FieldOptions where

