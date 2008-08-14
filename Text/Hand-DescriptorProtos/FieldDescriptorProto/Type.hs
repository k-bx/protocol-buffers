module Text.DescriptorProtos.FieldDescriptorProto.Type (Type(..))
       where
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Type = TYPE_DOUBLE
          | TYPE_FLOAT
          | TYPE_INT64
          | TYPE_UINT64
          | TYPE_INT32
          | TYPE_FIXED64
          | TYPE_FIXED32
          | TYPE_BOOL
          | TYPE_STRING
          | TYPE_GROUP
          | TYPE_MESSAGE
          | TYPE_BYTES
          | TYPE_UINT32
          | TYPE_ENUM
          | TYPE_SFIXED32
          | TYPE_SFIXED64
          | TYPE_SINT32
          | TYPE_SINT64
          deriving (P'.Show, P'.Read, P'.Eq, P'.Ord, P'.Data, P'.Typeable)
 
instance P'.Mergeable Type where
 
instance P'.Bounded Type where
        minBound = TYPE_DOUBLE
        maxBound = TYPE_SINT64
 
instance P'.Default Type where
        defaultValue = TYPE_DOUBLE
 
instance P'.Enum Type where
        fromEnum (TYPE_DOUBLE) = 1
        fromEnum (TYPE_FLOAT) = 2
        fromEnum (TYPE_INT64) = 3
        fromEnum (TYPE_UINT64) = 4
        fromEnum (TYPE_INT32) = 5
        fromEnum (TYPE_FIXED64) = 6
        fromEnum (TYPE_FIXED32) = 7
        fromEnum (TYPE_BOOL) = 8
        fromEnum (TYPE_STRING) = 9
        fromEnum (TYPE_GROUP) = 10
        fromEnum (TYPE_MESSAGE) = 11
        fromEnum (TYPE_BYTES) = 12
        fromEnum (TYPE_UINT32) = 13
        fromEnum (TYPE_ENUM) = 14
        fromEnum (TYPE_SFIXED32) = 15
        fromEnum (TYPE_SFIXED64) = 16
        fromEnum (TYPE_SINT32) = 17
        fromEnum (TYPE_SINT64) = 18
        toEnum 1 = TYPE_DOUBLE
        toEnum 2 = TYPE_FLOAT
        toEnum 3 = TYPE_INT64
        toEnum 4 = TYPE_UINT64
        toEnum 5 = TYPE_INT32
        toEnum 6 = TYPE_FIXED64
        toEnum 7 = TYPE_FIXED32
        toEnum 8 = TYPE_BOOL
        toEnum 9 = TYPE_STRING
        toEnum 10 = TYPE_GROUP
        toEnum 11 = TYPE_MESSAGE
        toEnum 12 = TYPE_BYTES
        toEnum 13 = TYPE_UINT32
        toEnum 14 = TYPE_ENUM
        toEnum 15 = TYPE_SFIXED32
        toEnum 16 = TYPE_SFIXED64
        toEnum 17 = TYPE_SINT32
        toEnum 18 = TYPE_SINT64
        succ (TYPE_DOUBLE) = TYPE_FLOAT
        succ (TYPE_FLOAT) = TYPE_INT64
        succ (TYPE_INT64) = TYPE_UINT64
        succ (TYPE_UINT64) = TYPE_INT32
        succ (TYPE_INT32) = TYPE_FIXED64
        succ (TYPE_FIXED64) = TYPE_FIXED32
        succ (TYPE_FIXED32) = TYPE_BOOL
        succ (TYPE_BOOL) = TYPE_STRING
        succ (TYPE_STRING) = TYPE_GROUP
        succ (TYPE_GROUP) = TYPE_MESSAGE
        succ (TYPE_MESSAGE) = TYPE_BYTES
        succ (TYPE_BYTES) = TYPE_UINT32
        succ (TYPE_UINT32) = TYPE_ENUM
        succ (TYPE_ENUM) = TYPE_SFIXED32
        succ (TYPE_SFIXED32) = TYPE_SFIXED64
        succ (TYPE_SFIXED64) = TYPE_SINT32
        succ (TYPE_SINT32) = TYPE_SINT64
        pred (TYPE_FLOAT) = TYPE_DOUBLE
        pred (TYPE_INT64) = TYPE_FLOAT
        pred (TYPE_UINT64) = TYPE_INT64
        pred (TYPE_INT32) = TYPE_UINT64
        pred (TYPE_FIXED64) = TYPE_INT32
        pred (TYPE_FIXED32) = TYPE_FIXED64
        pred (TYPE_BOOL) = TYPE_FIXED32
        pred (TYPE_STRING) = TYPE_BOOL
        pred (TYPE_GROUP) = TYPE_STRING
        pred (TYPE_MESSAGE) = TYPE_GROUP
        pred (TYPE_BYTES) = TYPE_MESSAGE
        pred (TYPE_UINT32) = TYPE_BYTES
        pred (TYPE_ENUM) = TYPE_UINT32
        pred (TYPE_SFIXED32) = TYPE_ENUM
        pred (TYPE_SFIXED64) = TYPE_SFIXED32
        pred (TYPE_SINT32) = TYPE_SFIXED64
        pred (TYPE_SINT64) = TYPE_SINT32
 
instance P'.Wire Type where
        wireSize 14 enum = P'.wireSize 14 (P'.fromEnum enum)
        wirePut 14 enum = P'.wirePut 14 (P'.fromEnum enum)
        wireGet 14 = P'.fmap P'.toEnum (P'.wireGet 14)
 
instance P'.ReflectEnum Type where
        reflectEnum
          = [(1, "TYPE_DOUBLE", TYPE_DOUBLE), (2, "TYPE_FLOAT", TYPE_FLOAT),
             (3, "TYPE_INT64", TYPE_INT64), (4, "TYPE_UINT64", TYPE_UINT64),
             (5, "TYPE_INT32", TYPE_INT32), (6, "TYPE_FIXED64", TYPE_FIXED64),
             (7, "TYPE_FIXED32", TYPE_FIXED32), (8, "TYPE_BOOL", TYPE_BOOL),
             (9, "TYPE_STRING", TYPE_STRING), (10, "TYPE_GROUP", TYPE_GROUP),
             (11, "TYPE_MESSAGE", TYPE_MESSAGE), (12, "TYPE_BYTES", TYPE_BYTES),
             (13, "TYPE_UINT32", TYPE_UINT32), (14, "TYPE_ENUM", TYPE_ENUM),
             (15, "TYPE_SFIXED32", TYPE_SFIXED32),
             (16, "TYPE_SFIXED64", TYPE_SFIXED64),
             (17, "TYPE_SINT32", TYPE_SINT32), (18, "TYPE_SINT64", TYPE_SINT64)]
        reflectEnumInfo _
          = P'.EnumInfo
              (P'.ProtoName "Text" "DescriptorProtos.FieldDescriptorProto"
                 "Type")
              [(1, "TYPE_DOUBLE"), (2, "TYPE_FLOAT"), (3, "TYPE_INT64"),
               (4, "TYPE_UINT64"), (5, "TYPE_INT32"), (6, "TYPE_FIXED64"),
               (7, "TYPE_FIXED32"), (8, "TYPE_BOOL"), (9, "TYPE_STRING"),
               (10, "TYPE_GROUP"), (11, "TYPE_MESSAGE"), (12, "TYPE_BYTES"),
               (13, "TYPE_UINT32"), (14, "TYPE_ENUM"), (15, "TYPE_SFIXED32"),
               (16, "TYPE_SFIXED64"), (17, "TYPE_SINT32"), (18, "TYPE_SINT64")]
{-module Text.DescriptorProtos.FieldDescriptorProto.Type
  (Type(..))
 where

import Text.ProtocolBuffers.Header

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
  deriving (Show,Read,Eq,Ord,Typeable,Data,Enum)

$( makeMergeableEnum ''Type )
-}