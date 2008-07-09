module DescriptorProtos.FieldDescriptorProto.Type
  (Type(..))
 where

import ProtocolBuffers.Header

data Type = TYPE_DOUBLE
          | TYPE_FLOAT
          | TYPE_INT64    -- inefficient for negative values
          | TYPE_UINT64
          | TYPE_INT32    -- inefficient for negative values
          | TYPE_FIXED64
          | TYPE_FIXED32
          | TYPE_BOOL
          | TYPE_STRING
          | TYPE_GROUP      -- Tag-delimited aggregate.
          | TYPE_MESSAGE    -- Length-delimeted aggegate
            -- descriptor.proto "New in version 2"
          | TYPE_BYTES
          | TYPE_UINT32
          | TYPE_ENUM
          | TYPE_SFIXED32
          | TYPE_SFIXED64
          | TYPE_SINT32 -- Uses ZipZag encoding
          | TYPE_SINT64 -- Uses ZipZag encoding
  deriving (Show,Eq,Ord,Typeable)

instance OptionFlag a => Monoid (Option a Type) where
  mempty = Absent
  mappend = flip const
