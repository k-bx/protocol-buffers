module B(B(..),bkeyca,bkeyac) where

import A(A)
import A(akeybc)
import C(C)
import C(ckeyba)

data B = B { name :: String }

bkeyca :: Either C (Maybe A)
bkeyca = Right Nothing

bkeyac :: Either A (Maybe C)
bkeyac = Right Nothing

instance Show B where
  show b = [name b,show ckeyba,show akeybc]
