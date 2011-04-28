module A(A(..),akeybc,akeycb) where

import B(B)
import B(bkeyac)
import C(C)
import C(ckeyab)

data A = A { name :: String }

akeybc :: Either B (Maybe C)
akeybc = Right Nothing

akeycb :: Either C (Maybe B)
akeycb = Right Nothing

instance Show A where
  show a = [name a,show bkeyac,show ckeyab]
