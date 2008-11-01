{- | 

"Text.ProtocolBuffers" exposes the client API.  This merely re-exports parts of the
other modules in protocol-buffers.  The exposed parts are:

@
import Text.ProtocolBuffers.Basic
  ( Seq,Utf8(..),ByteString,Int32,Int64,Word32,Word64
  , WireTag,FieldId,WireType,FieldType,EnumCode,WireSize
  , Mergeable(..),Default(..),Wire)
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.Extensions
  ( Key,ExtKey(getExt,putExt,clearExt),MessageAPI(..)
  , getKeyFieldId,getKeyFieldType,getKeyDefaultValue)
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),ProtoName(..),HsDefault(..),EnumInfoApp
  , KeyInfo,FieldInfo(..),DescriptorInfo(..),EnumInfo(..),ProtoInfo(..))
import Text.ProtocolBuffers.WireMessage
  ( Put,Get,runPut,runGet,runGetOnLazy
  , messageSize,messagePut,messageGet,messagePutM,messageGetM
  , messageWithLengthSize,messageWithLengthPut,messageWithLengthGet,messageWithLengthPutM,messageWithLengthGetM
  , messageAsFieldSize,messageAsFieldPutM,messageAsFieldGetM)
@

-}
module Text.ProtocolBuffers(
    module Text.ProtocolBuffers.Basic
  , module Text.ProtocolBuffers.Extensions
  , module Text.ProtocolBuffers.Identifiers
  , module Text.ProtocolBuffers.Reflections
  , module Text.ProtocolBuffers.WireMessage
  ) where

import Text.ProtocolBuffers.Basic
  ( Seq,Utf8(..),ByteString,Int32,Int64,Word32,Word64
  , WireTag,FieldId,WireType,FieldType,EnumCode,WireSize
  , Mergeable(..),Default(..),Wire)
import Text.ProtocolBuffers.Default()
import Text.ProtocolBuffers.Extensions
  ( Key,ExtKey(getExt,putExt,clearExt),MessageAPI(..)
  , getKeyFieldId,getKeyFieldType,getKeyDefaultValue)
import Text.ProtocolBuffers.Identifiers
import Text.ProtocolBuffers.Mergeable()
import Text.ProtocolBuffers.Reflections
  ( ReflectDescriptor(..),ReflectEnum(..),ProtoName(..),HsDefault(..),EnumInfoApp
  , KeyInfo,FieldInfo(..),DescriptorInfo(..),EnumInfo(..),ProtoInfo(..),makePNF )
import Text.ProtocolBuffers.WireMessage
  ( Put,Get,runPut,runGet,runGetOnLazy
  , messageSize,messagePut,messageGet,messagePutM,messageGetM
  , messageWithLengthSize,messageWithLengthPut,messageWithLengthGet,messageWithLengthPutM,messageWithLengthGetM
  , messageAsFieldSize,messageAsFieldPutM,messageAsFieldGetM)
