-- This provides much that is needed for the output of Gen.hs to compile against.
-- It will be imported qualified as P'
-- The prime ensuring no name conflicts are possible.
module Text.ProtocolBuffers.Header
    ( emptyBS -- needed for Gen.hs output
    , pack    -- needed for Gen.hs output
    , module Data.Generics
    , module Data.Typeable
    , module Text.ProtocolBuffers.Basic
    , module Text.ProtocolBuffers.Default
    , module Text.ProtocolBuffers.Mergeable
    , module Text.ProtocolBuffers.Reflections
    , module Text.ProtocolBuffers.WireMessage
    -- deprecated below
    , module Data.Monoid
    , module Data.DeriveTH
    , module Text.ProtocolBuffers.DeriveMergeable
    ) where

import Data.ByteString.Lazy(empty)
import Data.ByteString.Lazy.Char8(pack)
import Data.Dynamic(Dynamic)
import Data.Generics(Data(..))
import Data.Monoid(Monoid(..))
import Data.Typeable(Typeable(..))

import Text.ProtocolBuffers.Basic -- all
import Text.ProtocolBuffers.Default(Default(..))
import Text.ProtocolBuffers.Mergeable(Mergeable(..))
import Text.ProtocolBuffers.Reflections(ReflectDescriptor(..),ReflectEnum(..),EnumInfo(..),ProtoName(..))
import Text.ProtocolBuffers.WireMessage(Wire(..))

-- deprecated imports
import Data.DeriveTH
import Text.ProtocolBuffers.DeriveMergeable(makeMergeable,makeMergeableEnum)

emptyBS :: ByteString
emptyBS = Data.ByteString.Lazy.empty

