-- | This module add unknown field supprt to the library
--
-- This should support
--  1) Storing unknown bytestrings in messages
--     a) Mergeable
--     b) Default
--     c) Show
--  2) loading the unknown bytestrings into a (Map FieldId) from wire
--     a) If wiretypes differ this is an error so report it
--     b) Take extra care to ensure a _copy_ of the input is kept (?)
--  3) save unknown bytestring back to the wire
--  4) API ?
--      a) Provide ability to "wireGet" the data as a real type
--      b) clear the data
--      c) has any unkown data ?
--  5) Extend reflection to indicate presence of support for unkown data
--  6) Extend Options and command line to flag this
--  7) Extend hprotoc to add in this field
module Text.ProtocolBuffers.Unknown
  ( UnknownField(..),UnknownMessage(..),UnknownFieldValue(..),wireSizeUnknownField,wirePutUnknownField,loadUnknown
  ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as F
import Data.Generics
import Data.Map(Map)
import qualified Data.Map as M
import Data.Monoid(mappend)
import Data.Sequence(Seq,(|>))
import qualified Data.Sequence as Seq
import Data.Typeable

import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.WireMessage
import Text.ProtocolBuffers.Get as Get (Get,bytesRead)

err :: String -> b
err msg = error $ "Text.ProtocolBuffers.Unknown error\n"++msg

class UnknownMessage msg where
  getUnknownField :: msg -> UnknownField
  putUnknownField :: UnknownField -> msg -> msg

newtype UnknownField = UnknownField (Map FieldId UnknownFieldValue)
  deriving (Eq,Ord,Show,Read,Data,Typeable)

data UnknownFieldValue = UFV !WireType !(Seq ByteString)
  deriving (Eq,Ord,Show,Read,Data,Typeable)

instance Mergeable UnknownField where
  mergeEmpty = UnknownField M.empty
  mergeAppend (UnknownField m1) (UnknownField m2) = UnknownField (M.unionWith mergeUnknownFieldValue m1 m2)

mergeUnknownFieldValue :: UnknownFieldValue -> UnknownFieldValue -> UnknownFieldValue
mergeUnknownFieldValue (UFV wt1 s1) (UFV wt2 s2) =
  if wt1 /= wt2 then err $ "mergeUnknownFieldValue: WireType mismatch "++show (wt1,wt2)
    else UFV wt2 (mappend s1 s2)

instance Default UnknownField where
  defaultValue = UnknownField M.empty

-- | This is used by the generated code
wireSizeUnknownField :: UnknownField -> WireSize
wireSizeUnknownField (UnknownField m) = F.foldl' aSize 0 (M.assocs m)  where
  aSize old (fi,(UFV wt raw)) = old +
    let tagSize = size'Varint (getWireTag (mkWireTag fi wt))
    in F.foldl' (\oldVal new -> oldVal + L.length new) (fromIntegral (Seq.length raw) * tagSize) raw

-- | This is used by the generated code
wirePutUnknownField :: UnknownField -> Put
wirePutUnknownField (UnknownField m) = mapM_ aPut (M.assocs m) where
  aPut (fi,(UFV wt raw)) = F.mapM_ (\bs -> putVarUInt (getWireTag $ mkWireTag fi wt) >> putLazyByteString bs) raw

{-
getMessageUnknown :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
                  => (FieldId -> message -> Get message)
                  -> Get message
getMessageUnknown = getMessageWith loadUnknown

getBareMessageUnknown :: (Mergeable message, ReflectDescriptor message,Typeable message,ExtendMessage message)
                      => (FieldId -> message -> Get message)
                      -> Get message
getBareMessageUnknown = getBareMessageWith loadUnknown
-}
loadUnknown :: (Typeable a, UnknownMessage a) => FieldId -> WireType -> a -> Get a
loadUnknown fieldId wireType msg = do
  let (UnknownField uf) = getUnknownField msg
      badwt wt = do here <- bytesRead
                    fail $ "Conflicting wire types at byte position "++show here ++ " for unknown field of message: "++show (typeOf msg,fieldId,wireType,wt)
  case M.lookup fieldId uf of
    Nothing -> do
      bs <- wireGetFromWire fieldId wireType
      let v' = UFV wireType (Seq.singleton bs)
          uf' = M.insert fieldId v' uf
      seq v' $ seq uf' $ return $ putUnknownField (UnknownField uf') msg
    Just (UFV wt raw) | wt /= wireType -> badwt wt
                                    | otherwise -> do
      bs <- wireGetFromWire fieldId wireType
      let v' = UFV wt (raw |> bs)
          uf' = M.insert fieldId v' uf
      seq v' $ seq uf' $ return $ putUnknownField (UnknownField uf') msg
