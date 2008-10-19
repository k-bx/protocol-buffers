-- | A strong feature of the protocol-buffers package is that it does
-- not contain any structures defined by descriptor.proto!  This
-- prevents me hitting any annoying circular dependencies.  The
-- structures defined here are included in each module created by
-- 'hprotoc'.  They are optimized for use in code generation.
--
-- These values can be inspected at runtime by the user's code, but I
-- have yet to write much documentation.  Luckily the record field
-- names are somewhat descriptive.
--
module Text.ProtocolBuffers.Reflections
  ( ProtoName(..),ProtoInfo(..),DescriptorInfo(..),FieldInfo(..),KeyInfo
  , HsDefault(..),EnumInfo(..),EnumInfoApp
  , ReflectDescriptor(..),ReflectEnum(..),GetMessageInfo(..)
  ) where

import Text.ProtocolBuffers.Basic

import Data.List(sort)
import qualified Data.Foldable as F(toList)
import Data.Set(Set)
import qualified Data.Set as Set(fromDistinctAscList)
import Data.Generics(Data)
import Data.Typeable(Typeable)
import Data.Map(Map)

-- | This is fully qualified name data type for code generation.  The
-- 'haskellPrefix' was possibly specified on the 'hprotoc' command
-- line.  The 'parentModule' is a combination of the module prefix
-- from the '.proto' file and any nested levels of definition.
--
-- The name components are likely to have been mangled to ensure the
-- 'baseName' started with an uppercase letter, in @ ['A'..'Z'] @.
data ProtoName = ProtoName { protobufName :: Utf8     -- ^ fully qualified name using "package" prefix (no mangling)
                           , haskellPrefix :: String  -- ^ Haskell specific prefix to module hierarchy (e.g. Text.Foo)
                           , parentModule :: String   -- ^ Proto specified namespace (like Com.Google.Bar)
                           , baseName :: String       -- ^ unqualfied name of this thing (with no periods)
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data ProtoInfo = ProtoInfo { protoMod :: ProtoName
                           , protoFilePath :: [FilePath]
                           , protoSource :: String
                           , extensionKeys :: Seq KeyInfo
                           , messages :: [DescriptorInfo]
                           , enums :: [EnumInfo]
                           , knownKeyMap :: Map ProtoName (Seq FieldInfo) -- All keys in namespace of protoFilePath
                           }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data DescriptorInfo = DescriptorInfo { descName :: ProtoName
                                     , descFilePath :: [FilePath]
                                     , isGroup :: Bool
                                     , fields :: Seq FieldInfo 
                                     , keys :: Seq KeyInfo
                                     , extRanges :: [(FieldId,FieldId)]
                                     , knownKeys :: Seq FieldInfo
                                     , storeUnknown :: Bool
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

-- | 'GetMessageInfo' is used in getting messages from the wire.  It
-- supplies the 'Set' of precomposed wire tags that must be found in
-- the message as well as a 'Set' of all allowed tags (including known
-- extension fields and all required wire tags).
--
-- Extension fields not in the allowedTags set are still loaded, but
-- only as 'ByteString' blobs that will have to interpreted later.
data GetMessageInfo = GetMessageInfo { requiredTags :: Set WireTag
                                     , allowedTags :: Set WireTag
                                     }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type KeyInfo = (ProtoName,FieldInfo) -- Extendee and FieldInfo

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
-- "Language.Haskell.Exts.Syntax" code generation by 'hprotoc'.
--
-- Note that Utf8 labeled byte sequences have been stripped to just
-- 'ByteString' here as this is sufficient for code generation.
data HsDefault = HsDef'Bool Bool
               | HsDef'ByteString ByteString
               | HsDef'Rational Rational
               | HsDef'Integer Integer
               | HsDef'Enum String
  deriving (Show,Read,Eq,Ord,Data,Typeable)

data EnumInfo = EnumInfo { enumName :: ProtoName
                         , enumFilePath :: [FilePath]
                         , enumValues :: [(EnumCode,String)]
                         }
  deriving (Show,Read,Eq,Ord,Data,Typeable)

type EnumInfoApp e = [(EnumCode,String,e)]

class ReflectEnum e where
  reflectEnum :: EnumInfoApp e
  reflectEnumInfo :: e -> EnumInfo            -- ^ Must not inspect argument
  parentOfEnum :: e -> Maybe DescriptorInfo   -- ^ Must not inspect argument
  parentOfEnum _ = Nothing

class ReflectDescriptor m where
  -- | This is obtained via 'read' on the stored 'show' output of the 'DescriptorInfo' in
  -- the module file. It is used in getting messages from the wire.
  -- 
  -- Must not inspect argument
  getMessageInfo :: m -> GetMessageInfo
  getMessageInfo x = cached
    where cached = makeMessageInfo (reflectDescriptorInfo (undefined `asTypeOf` x))
          makeMessageInfo :: DescriptorInfo -> GetMessageInfo
          makeMessageInfo di = GetMessageInfo { requiredTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di), isRequired f]
                                              , allowedTags = Set.fromDistinctAscList . sort $
                                                  [ wireTag f | f <- F.toList (fields di)] ++
                                                  [ wireTag f | f <- F.toList (knownKeys di)]
                                              }
  reflectDescriptorInfo :: m -> DescriptorInfo    -- ^ Must not inspect argument
