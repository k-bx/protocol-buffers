-- To get the defaults working sanely, I need to encode at least some reflection information.
-- The to-be-bootstrapped descriptor.proto structures are not parsed enough for sane default usage.
-- So this is currently over-designed for the immediate need of DescriptorInfo -> number -> FieldInfo -> Maybe HsDefault.
-- These data structures and API are quite likely to be rewritten.
--
-- A strong feature of this is that it does not contain any structures defined by descriptor.proto!
-- This prevents me hitting any circular dependencies
module Text.ProtocolBuffers.Reflections(ProtoName(..),DescriptorInfo(..),FieldInfo(..)
                                       ,HsDefault(..),EnumInfo(..),EnumInfoApp
                                       ,ReflectDescriptor(..),ReflectEnum(..)
                                       ) where

import Data.Map(Map)
import Text.ProtocolBuffers.Basic
import Data.Generics(Data)
import Data.Typeable(Typeable)

data ProtoName = ProtoName { haskellPrefix :: String  -- Haskell specific prefix to module hierarchy (e.g. Text)
                           , parentModule :: String   -- Proto specified namespace (like java)
                           , baseName :: String       -- unqualfied name of this thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , fields :: Map MyInt32 FieldInfo }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data FieldInfo = FieldInfo { name :: String
                           , number :: MyInt32
                           , isRequired :: Bool
                           , canRepeat :: Bool
                           , typeCode :: MyInt32
                           , type_name :: Maybe (Either DescriptorInfo EnumInfo)
                           , hsRawDefult :: Maybe ByteString -- crappy, perhaps escaped, thing
                           , hsDefault :: Maybe HsDefault    -- nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'Double Double
               | HsDef'Float Float
               | HsDef'Integer Integer
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumItems :: [(MyInt32,String)]
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(MyInt32,String,e)]

class ReflectDescriptor m where
  reflectDescriptor :: m -> Map MyInt32 FieldInfo   -- Must not inspect argument
  parentOfDescriptor :: m -> Maybe DescriptorInfo   -- Must not inspect argument
  parentOfDescriptor _ = Nothing

class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- Must not inspect argument
  parentOfEnum _ = Nothing

