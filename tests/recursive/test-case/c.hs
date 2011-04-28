module C(C(..),ckeyab,ckeyba) where

import A(A)
import A(akeycb)
import B(B)
import B(bkeyca)

data C = C { name :: String }

ckeyab :: Either A (Maybe B)
ckeyab = Right Nothing

ckeyba :: Either B (Maybe A)
ckeyba = Right Nothing

instance Show C where
  show c = [name c,show akeycb,show bkeyca]
