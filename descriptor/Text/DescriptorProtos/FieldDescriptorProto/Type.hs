module Text.DescriptorProtos.FieldDescriptorProto.Type (Type(..)) where
import Prelude ((+))
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
          deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Type
 
instance P'.Bounded Type where
  minBound = TYPE_DOUBLE
  maxBound = TYPE_SINT64
 
instance P'.Default Type where
  defaultValue = TYPE_DOUBLE
 
toMaybe'Enum :: P'.Int -> P'.Maybe Type
toMaybe'Enum 1 = P'.Just TYPE_DOUBLE
toMaybe'Enum 2 = P'.Just TYPE_FLOAT
toMaybe'Enum 3 = P'.Just TYPE_INT64
toMaybe'Enum 4 = P'.Just TYPE_UINT64
toMaybe'Enum 5 = P'.Just TYPE_INT32
toMaybe'Enum 6 = P'.Just TYPE_FIXED64
toMaybe'Enum 7 = P'.Just TYPE_FIXED32
toMaybe'Enum 8 = P'.Just TYPE_BOOL
toMaybe'Enum 9 = P'.Just TYPE_STRING
toMaybe'Enum 10 = P'.Just TYPE_GROUP
toMaybe'Enum 11 = P'.Just TYPE_MESSAGE
toMaybe'Enum 12 = P'.Just TYPE_BYTES
toMaybe'Enum 13 = P'.Just TYPE_UINT32
toMaybe'Enum 14 = P'.Just TYPE_ENUM
toMaybe'Enum 15 = P'.Just TYPE_SFIXED32
toMaybe'Enum 16 = P'.Just TYPE_SFIXED64
toMaybe'Enum 17 = P'.Just TYPE_SINT32
toMaybe'Enum 18 = P'.Just TYPE_SINT64
toMaybe'Enum _ = P'.Nothing
 
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
  toEnum
   = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FieldDescriptorProto.Type") P'..
      toMaybe'Enum
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
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
 
instance P'.GPB Type
 
instance P'.MessageAPI msg' (msg' -> Type) Type where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Type where
  reflectEnum
   = [(1, "TYPE_DOUBLE", TYPE_DOUBLE), (2, "TYPE_FLOAT", TYPE_FLOAT), (3, "TYPE_INT64", TYPE_INT64),
      (4, "TYPE_UINT64", TYPE_UINT64), (5, "TYPE_INT32", TYPE_INT32), (6, "TYPE_FIXED64", TYPE_FIXED64),
      (7, "TYPE_FIXED32", TYPE_FIXED32), (8, "TYPE_BOOL", TYPE_BOOL), (9, "TYPE_STRING", TYPE_STRING),
      (10, "TYPE_GROUP", TYPE_GROUP), (11, "TYPE_MESSAGE", TYPE_MESSAGE), (12, "TYPE_BYTES", TYPE_BYTES),
      (13, "TYPE_UINT32", TYPE_UINT32), (14, "TYPE_ENUM", TYPE_ENUM), (15, "TYPE_SFIXED32", TYPE_SFIXED32),
      (16, "TYPE_SFIXED64", TYPE_SFIXED64), (17, "TYPE_SINT32", TYPE_SINT32), (18, "TYPE_SINT64", TYPE_SINT64)]
  reflectEnumInfo _
   = P'.EnumInfo
      (P'.makePNF (P'.pack ".google.protobuf.FieldDescriptorProto.Type") ["Text"] ["DescriptorProtos", "FieldDescriptorProto"]
        "Type")
      ["Text", "DescriptorProtos", "FieldDescriptorProto", "Type.hs"]
      [(1, "TYPE_DOUBLE"), (2, "TYPE_FLOAT"), (3, "TYPE_INT64"), (4, "TYPE_UINT64"), (5, "TYPE_INT32"), (6, "TYPE_FIXED64"),
       (7, "TYPE_FIXED32"), (8, "TYPE_BOOL"), (9, "TYPE_STRING"), (10, "TYPE_GROUP"), (11, "TYPE_MESSAGE"), (12, "TYPE_BYTES"),
       (13, "TYPE_UINT32"), (14, "TYPE_ENUM"), (15, "TYPE_SFIXED32"), (16, "TYPE_SFIXED64"), (17, "TYPE_SINT32"),
       (18, "TYPE_SINT64")]