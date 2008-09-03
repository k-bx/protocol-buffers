-- To get the defaults working sanely, I need to encode at least some reflection information.
-- The to-be-bootstrapped descriptor.proto structures are not parsed enough for sane default usage.
-- So this is currently over-designed for the immediate need of DescriptorInfo -> number -> FieldInfo -> Maybe HsDefault.
-- These data structures and API are quite likely to be rewritten.
--
-- A strong feature of this is that it does not contain any structures defined by descriptor.proto!
-- This prevents me hitting any circular dependencies.
--
-- 
module Text.ProtocolBuffers.Reflections(ProtoName(..),ProtoInfo(..),DescriptorInfo(..),FieldInfo(..),KeyInfo
                                       ,HsDefault(..),EnumInfo(..),EnumInfoApp
                                       ,ReflectDescriptor(..),ReflectEnum(..),GetMessageInfo(..)
                                       ) where

import Text.ProtocolBuffers.Basic

import Data.List(sort)
import qualified Data.Foldable as F(toList)
import Data.Set(Set)
import qualified Data.Set as Set(fromDistinctAscList)
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Data.Map(Map)

data ProtoName = ProtoName { haskellPrefix :: String  -- Haskell specific prefix to module hierarchy (e.g. Text)
                           , parentModule :: String   -- Proto specified namespace (like java)
                           , baseName :: String       -- unqualfied name of this thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data ProtoInfo = ProtoInfo { protoMod :: ProtoName
                           , protoFilePath :: [FilePath]
                           , extensionKeys :: Seq KeyInfo
                           , messages :: [DescriptorInfo]
                           , enums :: [EnumInfo]
                           , knownKeyMap :: Map ProtoName (Seq FieldInfo)
                           }

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , descFilePath :: [FilePath]
                                     , isGroup :: Bool
                                     , fields :: Seq FieldInfo 
                                     , keys :: Seq KeyInfo
                                     , extRanges :: [(FieldId,FieldId)]
                                     , knownKeys :: Seq FieldInfo
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data GetMessageInfo = GetMessageInfo { requiredTags :: Set WireTag
                                     , allowedTags :: Set WireTag
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type KeyInfo = (ProtoName,FieldInfo)

data FieldInfo = FieldInfo { fieldName     :: ProtoName
                           , fieldNumber   :: FieldId
                           , wireTag       :: WireTag
                           , wireTagLength :: WireSize         -- ^ Bytes required in the Varint formatted wireTag
                           , isRequired    :: Bool
                           , canRepeat     :: Bool
                           , typeCode      :: FieldType        -- ^ fromEnum of Text.DescriptorProtos.FieldDescriptorProto.Type
                           , typeName      :: Maybe ProtoName  -- ^ Set for Messages,Groups,and Enums
                           , hsRawDefault  :: Maybe ByteString -- ^ crappy, but not escaped, thing
                           , hsDefault     :: Maybe HsDefault  -- ^ nice parsed thing
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

-- | 'HsDefault' stores the parsed default from the proto file in a
-- form that will make a nice literal in the
-- Language.Haskell.Exts.Syntax sense.
data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'Rational Rational
               | HsDef'Integer Integer
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumFilePath :: [FilePath]
                         , enumValues :: [(EnumCode,String)]
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(EnumCode,String,e)]

class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- Must not inspect argument
  parentOfEnum _ = Nothing

class ReflectDescriptor m where
  getMessageInfo :: m -> GetMessageInfo           -- Must not inspect argument
  getMessageInfo x = cached
    where cached = makeMessageInfo (reflectDescriptorInfo (undefined `asTypeOf` x))
          makeMessageInfo :: DescriptorInfo -> GetMessageInfo
          makeMessageInfo di = GetMessageInfo { requiredTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di), isRequired f]
                                              , allowedTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di)] }
  reflectDescriptorInfo :: m -> DescriptorInfo    -- Must not inspect argument
  parentOfDescriptor :: m -> Maybe DescriptorInfo -- Must not inspect argument
  parentOfDescriptor _ = Nothing

