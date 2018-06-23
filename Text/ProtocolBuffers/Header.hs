-- | This provides what is needed for the output of 'hprotoc' to
-- compile.  This and the Prelude will both be imported qualified as
-- P', the prime ensuring no name conflicts are possible.
module Text.ProtocolBuffers.Header
    ( append, emptyBS
    , pack, fromMaybe, ap
    , fromDistinctAscList, member
    , throwError,catchError
    , choice, sepEndBy, spaces, try
    , module Data.Generics
    , module Text.ProtocolBuffers.Basic
    , module Text.ProtocolBuffers.Extensions
    , module Text.ProtocolBuffers.Identifiers
    , module Text.ProtocolBuffers.Reflections
    , module Text.ProtocolBuffers.TextMessage
    , module Text.ProtocolBuffers.Unknown
    , module Text.ProtocolBuffers.WireMessage
    ) where

import Control.Monad(ap)
import Control.Monad.Error.Class(throwError,catchError)
import Data.ByteString.Lazy(empty)
import Data.ByteString.Lazy.Char8(pack)
import Data.Generics(Data(..))
import Data.Maybe(fromMaybe)
import Data.Sequence((|>)) -- for append, see below
import Data.Set(fromDistinctAscList,member)
import Text.Parsec(choice, sepEndBy, spaces, try)

import Text.ProtocolBuffers.Basic -- all
import Text.ProtocolBuffers.Extensions
  ( wireSizeExtField,wirePutExtField,wirePutExtFieldWithSize
  , loadExtension,notExtension
  , wireGetKeyToUnPacked, wireGetKeyToPacked
  , GPB,Key(..),ExtField,ExtendMessage(..),MessageAPI(..),ExtKey(wireGetKey),PackedSeq )
import Text.ProtocolBuffers.Identifiers(FIName(..),MName(..),FName(..))
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),EnumInfo(..),ProtoName(..)
  , GetMessageInfo(GetMessageInfo),DescriptorInfo(extRanges),makePNF )
import Text.ProtocolBuffers.TextMessage -- all
import Text.ProtocolBuffers.Unknown
  ( UnknownField,UnknownMessage(..),wireSizeUnknownField,wirePutUnknownField,catch'Unknown )
import Text.ProtocolBuffers.WireMessage
  ( Wire(..)
  , prependMessageSize,putSize,splitWireTag
  , runPutM
  , wireSizeReq,wireSizeOpt,wireSizeRep
  , wirePutReq,wirePutOpt,wirePutRep
  , wirePutReqWithSize,wirePutOptWithSize,wirePutRepWithSize
  , sequencePutWithSize
  , wirePutPacked,wirePutPackedWithSize,wireSizePacked
  , getMessageWith,getBareMessageWith,wireGetEnum,wireGetPackedEnum
  , wireSizeErr,wirePutErr,wireGetErr,size'WireSize
  , unknown,unknownField
  , fieldIdOf)

{-# INLINE append #-}
append :: Seq a -> a -> Seq a
append = (|>)

{-# INLINE emptyBS #-}
emptyBS :: ByteString
emptyBS = Data.ByteString.Lazy.empty
