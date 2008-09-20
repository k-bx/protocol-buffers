-- | This provides much that is needed for the output of 'hprotoc' to
-- compile.  It will be imported qualified as P', the prime ensuring
-- no name conflicts are possible.
module Text.ProtocolBuffers.Header
    ( -- needed for Gen.hs output
      emptyBS
    , pack
    , append
    , module Control.Monad
    , module Data.Generics
    , module Data.Typeable
    , module Text.ProtocolBuffers.Basic
    , module Text.ProtocolBuffers.Extensions
    , module Text.ProtocolBuffers.Reflections
    , module Text.ProtocolBuffers.WireMessage
    ) where

import Control.Monad(ap)
import Data.ByteString.Lazy(empty)
import Data.ByteString.Lazy.Char8(pack)
import Data.Generics(Data(..))
import Data.Sequence((|>)) -- for append, see below
import Data.Typeable(Typeable(..))

import Text.ProtocolBuffers.Basic -- all
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.Extensions(wireSizeExtField,wirePutExtField,GPB,getMessageExt,getBareMessageExt,Key(..),ExtField,ExtendMessage(..),MessageAPI(..),ExtKey(wireGetKey))
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections(ReflectDescriptor(..),ReflectEnum(..),EnumInfo(..),ProtoName(..),DescriptorInfo(extRanges))
import Text.ProtocolBuffers.WireMessage( prependMessageSize,putSize
                                       , wireSizeReq,wireSizeOpt,wireSizeRep
                                       , wirePutReq,wirePutOpt,wirePutRep
                                       , getMessage,getBareMessage
                                       , wireSizeErr,wirePutErr,wireGetErr
                                       , unknownField)

append :: Seq a -> a -> Seq a
append = (|>)

emptyBS :: ByteString
emptyBS = Data.ByteString.Lazy.empty
