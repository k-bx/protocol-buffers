{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Text.ProtocolBuffers.GRPC
  ( makeServiceHandlers,
    makeGrpcService,
    makeGrpcApp,
    Compression,
    uncompressed,
    gzip,
    QualifiedMethod (..),
    QualifiedMethods,
    qualifiedMethods,
    ServiceHandler (..),
    StreamHandler (..),
    UnaryHandler,
    ServerStreamHandler,
    ServerStream (..),
    ClientStreamHandler,
    ClientStream (..),
    BiDiStreamHandler,
    BiDiStream (..),
    BiDiStep (..),
    GeneralStreamHandler,
    IncomingStream (..),
    OutgoingStream (..),
  )
where

import Data.Binary.Builder (fromByteString, putWord32be, singleton)
import Data.Binary.Get (getByteString, getInt8, getWord32be, runGetIncremental)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Builder as BSLB
import Data.HVect hiding (singleton)
import Data.Proxy
import GHC.Exts
import GHC.TypeLits
import Network.GRPC.HTTP2.Encoding
import Network.GRPC.HTTP2.Types
import Network.GRPC.Server.Handlers
import Network.GRPC.Server.Wai (ServiceHandler (..))
import qualified Network.GRPC.Server.Wai
import Network.Wai
import Text.ProtocolBuffers (Method, ReflectDescriptor, Service, Streaming (..), Wire, messageGet)
import Text.ProtocolBuffers.Basic (Service (Service))
import Text.ProtocolBuffers.WireMessage (messagePut)

data QualifiedMethod (serviceName :: Symbol) (methodName :: Symbol) (input :: Streaming *) (output :: Streaming *) = QualifiedMethod

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

class HasQualifiedMethods service where
  type QualifiedMethods service :: [*]
  qualifiedMethods :: service -> HVect (QualifiedMethods service)

instance HasQualifiedMethods (Service serviceName '[]) where
  type QualifiedMethods (Service serviceName '[]) = '[]
  qualifiedMethods _ = HNil

instance HasQualifiedMethods (Service serviceName rest) => HasQualifiedMethods (Service serviceName (Method methodName i o ': rest)) where
  type QualifiedMethods (Service serviceName (Method methodName i o ': rest)) = QualifiedMethod serviceName methodName i o ': QualifiedMethods (Service serviceName rest)
  qualifiedMethods _ = QualifiedMethod :&: qualifiedMethods (Service :: Service serviceName rest)

class MakeHandlers (methods :: [*]) a where
  type MakeHandlersResult methods a
  makeHandlers :: KnownSymbol serviceName => proxy serviceName -> Proxy methods -> ([ServiceHandler] -> a) -> MakeHandlersResult methods a

instance MakeHandlers '[] a where
  type MakeHandlersResult '[] a = a
  makeHandlers _ _ acc = acc []

data StreamHandler m (i :: Streaming *) (o :: Streaming *) where
  UnaryHandler :: (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => UnaryHandler m i o -> StreamHandler m (Single i) (Single o)
  ServerStreamHandler :: (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => ServerStreamHandler m i o a -> StreamHandler m (Single i) (StreamOf o)
  ClientStreamHandler :: (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => ClientStreamHandler m i o a -> StreamHandler m (StreamOf i) (Single o)
  BiDiStreamHandler :: (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => BiDiStreamHandler m i o a -> StreamHandler m (StreamOf i) (StreamOf o)
  GeneralStreamHandler :: (Wire i, Wire o, ReflectDescriptor i, ReflectDescriptor o) => GeneralStreamHandler m i o a b -> StreamHandler m (StreamOf i) (StreamOf o)

instance (MakeHandlers xs a, KnownSymbol methodName) => MakeHandlers (Method methodName i o ': xs) a where
  type MakeHandlersResult (Method methodName i o ': xs) a = StreamHandler IO i o -> MakeHandlersResult xs a
  makeHandlers (serviceName :: proxy serviceName) _ acc handler =
    let method = QualifiedMethod :: QualifiedMethod serviceName methodName i o
        newEntry =
          case handler of
            UnaryHandler handler -> unary method handler
            ServerStreamHandler handler -> serverStream method handler
            ClientStreamHandler handler -> clientStream method handler
            BiDiStreamHandler handler -> bidiStream method handler
            GeneralStreamHandler handler -> generalStream method handler
     in makeHandlers serviceName (Proxy :: Proxy xs) (acc . (newEntry :))

makeServiceHandlers :: forall methods serviceName. (MakeHandlers methods [ServiceHandler], KnownSymbol serviceName) => Service serviceName methods -> MakeHandlersResult methods [ServiceHandler]
makeServiceHandlers service = withServiceHandlers service id

makeGrpcService :: forall methods serviceName. (MakeHandlers methods (Application -> Application), KnownSymbol serviceName) => Service serviceName methods -> [Compression] -> MakeHandlersResult methods (Application -> Application)
makeGrpcService service compression = withServiceHandlers service $ Network.GRPC.Server.Wai.grpcService compression

makeGrpcApp :: forall methods serviceName. (MakeHandlers methods Application, KnownSymbol serviceName) => Service serviceName methods -> [Compression] -> MakeHandlersResult methods Application
makeGrpcApp service compression = withServiceHandlers service $ Network.GRPC.Server.Wai.grpcApp compression

withServiceHandlers :: forall methods serviceName res. (MakeHandlers methods res, KnownSymbol serviceName) => Service serviceName methods -> ([ServiceHandler] -> res) -> MakeHandlersResult methods res
withServiceHandlers _ = makeHandlers (Proxy :: Proxy serviceName) (Proxy :: Proxy methods)
