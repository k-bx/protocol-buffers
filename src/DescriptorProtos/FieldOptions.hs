module DescriptorProtos.FieldOptions
  (FieldOptions(..))
 where

import ProtocolBuffers.Header

import qualified DescriptorProtos.FieldOptions.CType as DescriptorProtos.FieldOptions(CType)

data FieldOptions = FieldOptions
    { ctype :: Maybe DescriptorProtos.FieldOptions.CType
    , experimental_map_key :: Maybe String
    }
