{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Werror #-}
module Text.ProtocolBuffers.ProtoJSON where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as V
import Text.ProtocolBuffers.Basic
import Text.Read (readEither)

import qualified Data.ByteString.Base16.Lazy as BL16
import qualified Data.Text.Lazy.Encoding as TL

{-# INLINE objectNoEmpty #-}
objectNoEmpty :: [Pair] -> Value
objectNoEmpty = object . filter (hasContent . snd)
    where
      hasContent Null = False
      hasContent (Array xs) | V.null xs = False
      hasContent _ = True

{-# INLINE toJSONShow #-}
toJSONShow :: Show a => a -> Value
toJSONShow = toJSON . show

{-# INLINE parseJSONEnum #-}
parseJSONEnum :: Read a => String -> Value -> Parser a
parseJSONEnum name x = do
    t <- parseJSON x
    case readEither t of
      Left _ -> fail $ "Invalid value "++show t++" for enum "++name
      Right res -> return res

{-# INLINE toJSONShowWithPayload #-}
toJSONShowWithPayload :: Show a => a -> Value
toJSONShowWithPayload x = object [("payload", toJSON . show $ x)]

{-# INLINE parseJSONReadWithPayload #-}
parseJSONReadWithPayload :: Read a => String -> Value -> Parser a
parseJSONReadWithPayload tyName = withObject tyName $ \o -> do
    t <- o .: "payload"
    case readEither t of
      Left e -> fail e
      Right res -> return res

{-# INLINE parseJSONBool #-}
parseJSONBool :: Value -> Parser Bool
parseJSONBool (Bool b) = return b
parseJSONBool (Number sci) = return (sci >= 1)
parseJSONBool _ = fail "Expected Bool"

{-# INLINE toJSONByteString #-}
toJSONByteString :: ByteString -> Value
toJSONByteString bs = object [("payload", toJSON . TL.decodeUtf8 . BL16.encode $ bs)]

{-# INLINE parseJSONByteString #-}
parseJSONByteString :: Value -> Parser ByteString
parseJSONByteString = withObject "bytes" $ \o -> do
    t <- o .: "payload"
    case BL16.decode (TL.encodeUtf8 t) of
      (bs, "") -> return bs
      _ -> fail "Failed to parse base16."
