module Text.DescriptorProtos.FieldOptions
  (FieldOptions(..))
 where

import Text.ProtocolBuffers.Header

import qualified Text.DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions(CType)

data FieldOptions = FieldOptions
    { ctype :: Maybe DescriptorProtos.FieldOptions.CType
    , experimental_map_key :: Maybe ByteString
    }
  deriving (Show,Eq,Ord,Typeable)

$( makeMergeable ''FieldOptions )

instance Default FieldOptions where

