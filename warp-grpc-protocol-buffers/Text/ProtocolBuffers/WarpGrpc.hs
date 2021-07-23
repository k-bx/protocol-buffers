{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.ProtocolBuffers.WarpGrpc () where

import Data.Binary.Builder (fromByteString, putWord32be, singleton)
import Data.Binary.Get (getByteString, getInt8, getWord32be, runGetIncremental)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSLB
import Data.Proxy
import GHC.TypeLits
import Network.GRPC.HTTP2.Encoding
import Network.GRPC.HTTP2.Types
import Network.GRPC.Server.Wai
import Text.ProtocolBuffers (Method, ReflectDescriptor, Service, Wire, messageGet, Streaming(..))
import Text.ProtocolBuffers.WireMessage (messagePut)
import Network.GRPC.Server.Handlers

data QualifiedMethod (serviceName :: Symbol) (methodName :: Symbol) (input :: Streaming *) (output :: Streaming *)

instance (KnownSymbol serviceName, KnownSymbol methodName) => IsRPC (QualifiedMethod serviceName methodName input output) where
  path rpc = BSC.pack $ "/" <> symbolVal (Proxy :: Proxy serviceName) <> "/" <> symbolVal (Proxy :: Proxy methodName)
  {-# INLINE path #-}

instance (Wire i, ReflectDescriptor i, KnownSymbol serviceName, KnownSymbol methodName) => GRPCInput (QualifiedMethod serviceName methodName (s i) output) i where
  encodeInput _ = encode
  decodeInput _ = decoder

instance (Wire o, ReflectDescriptor o, KnownSymbol serviceName, KnownSymbol methodName) => GRPCOutput (QualifiedMethod serviceName methodName input (s o)) o where
  encodeOutput _ = encode
  decodeOutput _ = decoder

encode :: (Wire m, ReflectDescriptor m) => Compression -> m -> Builder
encode compression plain =
  mconcat
    [ singleton (if _compressionByteSet compression then 1 else 0),
      putWord32be (fromIntegral $ BS.length bin),
      fromByteString bin
    ]
  where
    bin = _compressionFunction compression . BSL.toStrict $ messagePut plain

decoder :: (Wire a, ReflectDescriptor a) => Compression -> Decoder (Either String a)
decoder compression =
  runGetIncremental $ do
    isCompressed <- getInt8
    let decompress = if isCompressed == 0 then pure else _decompressionFunction compression
    n <- getWord32be
    bs <- decompress =<< getByteString (fromIntegral n)
    return $
      case messageGet (BSL.fromStrict bs) of
        Left e -> Left e
        Right (x, _) -> Right x

class IsValidHandler (i :: Streaming *) (o :: Streaming *) handler where
  makeHandler :: (KnownSymbol serviceName, KnownSymbol methodName) => QualifiedMethod serviceName methodName i o -> handler -> ServiceHandler

instance (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => IsValidHandler (Single i) (Single o) (UnaryHandler IO i o) where
  makeHandler = unary

instance (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => IsValidHandler (Single i) (StreamOf o) (ServerStreamHandler IO i o a) where
  makeHandler = serverStream

instance (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => IsValidHandler (StreamOf i) (Single o) (ClientStreamHandler IO i o a) where
  makeHandler = clientStream

instance (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => IsValidHandler (StreamOf i) (StreamOf o) (BiDiStreamHandler IO i o a) where
  makeHandler = bidiStream

instance (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => IsValidHandler (StreamOf i) (StreamOf o) (GeneralStreamHandler IO i o a b) where
  makeHandler = generalStream
