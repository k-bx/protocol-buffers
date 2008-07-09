module ProtocolBuffers.Header (UArray,ByteString,Data(..),Int32,Int64,Seq,Typeable(..),Word32,Word64
                              ,module ProtocolBuffers.Option,module ProtocolBuffers.Default
                              ,module Data.Monoid,module Data.DeriveTH,module Data.Derive.Monoid) where

import Data.Array.Unboxed(UArray)
import Data.ByteString(ByteString)
import Data.Derive.Monoid
import Data.DeriveTH
import Data.Generics(Data(..))
import Data.Int(Int32,Int64)
import Data.Monoid(Monoid(..))
import Data.Sequence(Seq)
import Data.Typeable(Typeable(..))
import Data.Word(Word32,Word64)
import ProtocolBuffers.Option
import ProtocolBuffers.Default

--instance OptionFlag a => Monoid (Option a (UArray i e)) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Bool) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a ByteString) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Int64) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word32) where mempty = Absent; mappend = op'Last
instance OptionFlag a => Monoid (Option a Word64) where mempty = Absent; mappend = op'Last
