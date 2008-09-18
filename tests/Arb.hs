-- | The "Arb" module defined Arbitrary instances for all the basic types
module Arb where

import Test.QuickCheck
import System.Random
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.ProtocolBuffers.Basic
import Data.Word(Word8)

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

minmax :: Bounded a => (a,a)
minmax = (minBound,maxBound)

instance Arbitrary Int32 where
  arbitrary = choose minmax
  coarbitrary n = varaint (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Int32 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Int64 where
  arbitrary = choose minmax
  coarbitrary n = varaint (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Int64 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word32 where
  arbitrary = choose minmax
  coarbitrary n = varaint (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word32 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word64 where
  arbitrary = choose minmax
  coarbitrary n = varaint (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word64 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word8 where
  arbitrary = choose minmax
  coarbitrary n = varaint (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word8 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Char where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromEnum n) `rem` 4))

instance Arbitrary Utf8 where
  len <- frequency
           [ (5, choose (0,1000))
           , (1, return 0) ]
  fmap (Utf8  U.fromString) (vector len)

instance Arbitrary L.ByteString where
  len <- frequency
           [ (5, choose (1,1000))
           , (1, return 0) ]
  fmap L.pack (vector len)

instance Arbitrary a => Arbitrary (Seq a) where
  len <- frequency
           [ (5, choose (1,17))
           , (1, return 0) ]
  fmap Seq.fromList (vector len)
