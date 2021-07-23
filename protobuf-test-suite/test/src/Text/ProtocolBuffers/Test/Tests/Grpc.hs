{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
module Text.ProtocolBuffers.Test.Tests.Grpc
  ( grpcTests
  ) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?), (@?=), assertEqual)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck ()

import qualified Data.Aeson as J
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Applicative (liftA)
import Network.GRPC.Server.Wai

import Control.Exception
import Control.Monad.IO.Class
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header
import Text.ProtocolBuffers.TextMessage
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.GRPC
import Data.HVect
import Network.GRPC.Server.Handlers
import Network.Wai
import Network.GRPC.HTTP2.Encoding
import Network.GRPC.HTTP2.Types
import Network.Wai.Handler.Warp
import Network.GRPC.Client hiding (Abort)
import Network.HTTP2.Client
import Network.HTTP2.Client.FrameConnection

import HSCodeGen.Grpc.HelloService
import HSCodeGen.Grpc.HelloRequest
import HSCodeGen.Grpc.HelloResponse

grpcTests :: TestTree
grpcTests = testGroup "GRPC tests"
  [ testCase "handler paths" $
      map grpcHandlerPath handlers @?= ["/grpc.HelloService/SayHello","/grpc.HelloService/LotsOfReplies","/grpc.HelloService/LotsOfGreetings","/grpc.HelloService/BidiHello"]
  , testCase "client/server" $ withGprcServer $ \port -> doClientIO $ runClientTest port
  ]

withGprcServer = testWithApplication . return $ grpcApp [gzip] handlers

handlers :: [ServiceHandler]
handlers =
  makeServiceHandlers
    helloService
    (UnaryHandler $ \req i -> return $ makeResponse 0 i)
    (ServerStreamHandler $ \req i -> return (0, testServerStream i))
    (ClientStreamHandler $ \req -> return ((0, Nothing), testClientStream))
    (BiDiStreamHandler $ \req -> return (0, BiDiStream $ \_ -> return Abort))

doClientIO act = do
  res <- runClientIO act
  case res of
    Left e -> throwIO e
    Right x -> return x

runClientTest port = do
  connection <- newHttp2FrameConnection "127.0.0.1" (fromIntegral port) Nothing
  client <- newHttp2Client connection 1024 1024 [] defaultGoAwayHandler ignoreFallbackHandler
  let doRequest :: IsRPC r => Compression -> Compression -> RPCCall r a -> ClientIO a
      doRequest encoding decoding call = do
        res <- open client (BC.pack ("localhost:"<>show port)) [] (Timeout 10) (Encoding encoding) (Decoding decoding) call
        case res of
          Left e -> fail $ show e
          Right x -> return x
  liftIO . assertEqual "" (Right defaultValue{ reply = Just "(0,Just \"test\")" })
    =<< either (fail . show) (\(_, _, res) -> return res)
    =<< doRequest gzip gzip (singleRequest unaryMethod defaultValue{greeting = Just "test"})
  liftIO . assertEqual "" ([defaultValue{ reply = Just "(0,Just \"test\")" }, defaultValue{ reply = Just "(1,Just \"test\")" }]) . (\(res, _, _) -> res)
    =<< doRequest gzip gzip (streamReply serverStreamMethod [] defaultValue{greeting = Just "test"} $ \acc _ o -> return $ acc <> [o])

testServerStream i = ServerStream $ \n ->
  if n > 1 then return Nothing else return (Just (n+1, makeResponse n i))

testClientStream =
  ClientStream
  { clientStreamHandler = \(n,_) i -> return (n+1, Just i)
  , clientStreamFinalizer = \(n,Just i) -> return $ makeResponse n i
  }

makeResponse n i =
  defaultValue
  { reply = Just . uFromString $ show (n, greeting i)
  }

unaryMethod :&: serverStreamMethod :&: clientStreamMethod :&: bodiStreamMethod :&: HNil = qualifiedMethods helloService
