-- | The "Arb" module defined Arbitrary instances for all the basic types
module Arb where

import Test.QuickCheck
import System.Random
import qualified Data.Sequence as Seq
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Header(UnknownField)
import Data.Word(Word8)

instance Arbitrary UnknownField where arbitrary = return defaultValue

arb :: Gen a -> IO a
arb g = do
   s <- getStdGen
   let (s1,s2) = split s
   setStdGen s1
   let (i,s2') = random s2
   return $ generate i s2' g

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g =
  case randomR (fromIntegral a :: Integer,fromIntegral b :: Integer) g of
    (x,g) -> (fromIntegral x, g)

minmax :: Bounded a => (a,a)
minmax = (minBound,maxBound)

barb :: (Enum a,Bounded a) => Gen a
barb = elements [minBound..maxBound]

bcoarb :: Enum a => a -> Gen b -> Gen b
bcoarb a = variant ((fromEnum a) `rem` 4)

class ArbCon a x where
  futz :: a -> Gen x

instance ArbCon a a where futz = return

instance (Arbitrary a,ArbCon b x) => ArbCon (a -> b) x where
  futz f = arbitrary >>= futz . f

instance Arbitrary Int32 where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Int32 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Int64 where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Int64 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word32 where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word32 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word64 where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word64 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Word8 where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word8 where
  randomR = integralRandomR
  random = randomR minmax

instance Arbitrary Char where
  arbitrary = choose minmax
  coarbitrary n = variant (fromIntegral ((fromEnum n) `rem` 4))

instance Arbitrary Utf8 where
  arbitrary = do 
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap (Utf8 . U.fromString) (vector len)
  coarbitrary (Utf8 s) = variant (fromIntegral ((L.length s) `rem` 4))

instance Arbitrary L.ByteString where
  arbitrary = do
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap L.pack (vector len)
  coarbitrary s = variant (fromIntegral ((L.length s) `rem` 4))

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = do
    len <- frequency
             [ (3, choose (1,3))
             , (1, return 0) ]
    fmap Seq.fromList (vector len)
  coarbitrary s = variant ((Seq.length s) `rem` 4)


