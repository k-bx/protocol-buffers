-- | This provides what is needed for the output of 'hprotoc' to
-- compile.  This and the Prelude will both be imported qualified as
-- P', the prime ensuring no name conflicts are possible.
module Text.ProtocolBuffers.Header
    ( emptyBS, pack, append, fromMaybe, ap, fromDistinctAscList
    , module Data.Generics
    , module Data.Typeable
    , module Text.ProtocolBuffers.Basic
    , module Text.ProtocolBuffers.Extensions
    , module Text.ProtocolBuffers.Identifiers
    , module Text.ProtocolBuffers.Reflections
    , module Text.ProtocolBuffers.Unknown
    , module Text.ProtocolBuffers.WireMessage
    ) where

import Control.Monad(ap)
import Data.ByteString.Lazy(empty)
import Data.ByteString.Lazy.Char8(pack)
import Data.Generics(Data(..))
import Data.Maybe(fromMaybe)
import Data.Sequence((|>)) -- for append, see below
import Data.Set(fromDistinctAscList)
import Data.Typeable(Typeable(..))

import Text.ProtocolBuffers.Basic -- all
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.Extensions
  ( wireSizeExtField,wirePutExtField,loadExtension,notExtension,getMessageExt,getBareMessageExt
  , GPB,Key(..),ExtField,ExtendMessage(..),MessageAPI(..),ExtKey(wireGetKey) )
import Text.ProtocolBuffers.Identifiers(FIName(..),MName(..),FName(..))
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),EnumInfo(..),ProtoName(..)
  , GetMessageInfo(GetMessageInfo),DescriptorInfo(extRanges),makePNF )
import Text.ProtocolBuffers.Unknown
  ( UnknownField,UnknownMessage(..),wireSizeUnknownField,wirePutUnknownField,loadUnknown )
import Text.ProtocolBuffers.WireMessage
  ( prependMessageSize,putSize
  , wireSizeReq,wireSizeOpt,wireSizeRep
  , wirePutReq,wirePutOpt,wirePutRep
  , getMessage,getBareMessage
  , getMessageWith,getBareMessageWith,wireGetEnum
  , wireSizeErr,wirePutErr,wireGetErr
  , unknown,unknownField)

append :: Seq a -> a -> Seq a
append = (|>)

emptyBS :: ByteString
emptyBS = Data.ByteString.Lazy.empty
