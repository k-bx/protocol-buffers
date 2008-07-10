module Text.ProtocolBuffers.Header (
    UArray,
    ByteString,
    Data(..),
    Int32,
    Int64,Seq,
    Typeable(..),
    Word32,
    Word64
  ,module Text.ProtocolBuffers.Default
  ,module Data.Monoid
  ,module Data.DeriveTH
  ,module Text.ProtocolBuffers.Mergeable
  ,module Text.ProtocolBuffers.DeriveMergeable) where

import Data.Array.Unboxed(UArray)
import Data.ByteString(ByteString)
import Data.DeriveTH
import Data.Generics(Data(..))
import Data.Int(Int32,Int64)
import Data.Monoid(Monoid(..))
import Data.Sequence(Seq)
import Data.Typeable(Typeable(..))
import Data.Word(Word32,Word64)

import Text.ProtocolBuffers.Default
import Text.ProtocolBuffers.DeriveMergeable
import Text.ProtocolBuffers.Mergeable

{-
--instance OptionFlag a => Monoid (Option a (UArray i e)) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Bool) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a ByteString) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int64) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word64) where mempty = Absent; mappend = op'Last
-}