module Text.ProtocolBuffers.Header (
    UArray,
    ByteString,
    Data(..),
    Int32,MyInt32,
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

-- This is what an sint32 is in Haskell.  "Int" is more widespread,
-- but not guaranteed to be the right size.
type MyInt32 = Data.Int.Int32

myInt32 :: String
myInt32 = "Int32"

{-
--instance OptionFlag a => Monoid (Option a (UArray i e)) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Bool) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a ByteString) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int64) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word64) where mempty = Absent; mappend = op'Last
-}