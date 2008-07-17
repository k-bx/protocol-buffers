-- To get the defaults working sanely, I need to encode at least some reflection information.
-- The to-be-bootstrapped descriptor.proto structures are not parsed enough for sane default usage.
-- So this is currently over-designed for the immediate need of DescriptorInfo -> number -> FieldInfo -> Maybe HsDefault.
-- These data structures and API are quite likely to be rewritten.
module Text.ProtocolBuffers.Reflections(DescriptorInfo(..),FieldInfo(..),EnumInfo,EnumInfoApp) where

import Text.ProtocolBuffers.Header
import qualified Text.DescriptorProtos.FieldDescriptorProto.Label as DescriptorProtos.FieldDescriptorProto(Label)
import qualified Text.DescriptorProtos.FieldDescriptorProto.Type as DescriptorProtos.FieldDescriptorProto(Type)
import qualified Text.DescriptorProtos.FieldOptions as DescriptorProtos(FieldOptions)

data DescriptorInfo = DescriptorInfo { haskellPrefix :: String  -- Haskell specific prefix to module hierarchy
                                     , parentModule :: String   -- Proto specified namespace
                                     , name :: String           -- unqualfied name of this type
                                     , fields :: Map MyInt32 FieldInfo
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data FieldInfo = FieldInfo { name :: String
                           , number :: MyInt32
                           , label :: DescriptorProtos.FieldDescriptorProto.Label
                           , type' :: DescriptorProtos.FieldDescriptorProto.Type
                           , type_name :: Maybe 
                           , hsRawDefult :: Maybe ByteString -- crappy, perhaps escaped, thing
                           , hsDefault :: Maybe HsDefault    -- nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data HsDefault = HsDef'Bool Boolean
               | HsDef'ByteString ByteString
               | HsDef'Double Double
               | HsDef'Float Float
               | HsDef'Integer Integer
  deriving (Show,Read,Eq,Ord,Data,Typeable)

class ReflectDescriptor m where
  reflectDescriptor :: Map MyInt32 FieldInfo
  parentOfDescriptor :: Maybe DescriptorInfo 

type EnumInfo = [(Integer,String)]
type EnumInfoApp e = [(Integer,String,e)]

class ReflectEnum e where
  reflectEnums :: EnumInfoApp e
  reflectEnumInfo :: EnumInfo
  reflectEnumInfo = let eia :: EnumInfoApp
                        eia = reflectEnums
                    in map (\(a,b_) -> (a,b)) eia
  -- Must not inspect
  parentOfEnum :: e -> Maybe DescriptorInfo
  parentOfEnum _ = Nothing

