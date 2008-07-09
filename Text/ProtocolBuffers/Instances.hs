module Text.ProtocolBuffers.Instances(showsType,parseType) where

import Text.ParserCombinators.ReadP
import Text.DescriptorProtos.FieldDescriptorProto.Type(Type(..))

{-
instance Show Type where
  showsPrec _ = showsType

instance Read Type where
  readsPrec _ = readP_to_S readType
-}

showsType :: Type -> ShowS
showsType TYPE_DOUBLE s = "double" ++ s
showsType TYPE_FLOAT s = "float" ++ s
showsType TYPE_INT64 s = "int64" ++ s
showsType TYPE_UINT64 s = "uint64" ++ s
showsType TYPE_INT32  s = "int32" ++ s
showsType TYPE_FIXED64 s = "fixed64" ++ s
showsType TYPE_FIXED32 s = "fixed32" ++ s
showsType TYPE_BOOL s = "bool" ++ s
showsType TYPE_STRING s = "string" ++ s
showsType TYPE_GROUP s = "group" ++ s
showsType TYPE_MESSAGE s = "message" ++ s
showsType TYPE_BYTES s = "bytes" ++ s
showsType TYPE_UINT32 s = "uint32" ++ s
showsType TYPE_ENUM s = "enum" ++ s
showsType TYPE_SFIXED32 s = "sfixed32" ++ s
showsType TYPE_SFIXED64 s = "sfixed64" ++ s
showsType TYPE_SINT32 s = "sint32" ++ s
showsType TYPE_SINT64 s = "sint64" ++ s

parseType :: String -> Maybe Type
parseType s = case readP_to_S readType s of
                [(val,[])] -> Just val
                _ -> Nothing

readType :: ReadP Type
readType = choice [ return TYPE_DOUBLE << string "double"
                  , return TYPE_FLOAT << string "float"
                  , return TYPE_INT64 << string "int64"
                  , return TYPE_UINT64 << string "uint64"
                  , return TYPE_INT32  << string "int32"
                  , return TYPE_FIXED64 << string "fixed64"
                  , return TYPE_FIXED32 << string "fixed32"
                  , return TYPE_BOOL << string "bool"
                  , return TYPE_STRING << string "string"
                  , return TYPE_GROUP << string "group"
                  , return TYPE_MESSAGE << string "message"
                  , return TYPE_BYTES << string "bytes"
                  , return TYPE_UINT32 << string "uint32"
                  , return TYPE_ENUM << string "enum"
                  , return TYPE_SFIXED32 << string "sfixed32"
                  , return TYPE_SFIXED64 << string "sfixed64"
                  , return TYPE_SINT32 << string "sint32"
                  , return TYPE_SINT64 << string "sint64"
                  ]
  where (<<) = flip (>>)

