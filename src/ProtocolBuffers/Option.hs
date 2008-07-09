module ProtocolBuffers.Option(Option(..),OptionFlag(..),Optional,Require,NotRequired,Required,op'Last,op'Merge) where

import Data.Monoid(Monoid(..))

class OptionFlag a where
  isValid :: Option a b -> Bool

instance OptionFlag NotRequired where
  isValid _ = True
instance OptionFlag Required where
  isValid Absent = False
  isValid _ = True

data NotRequired
data Required

type Optional b = Option NotRequired b
type Require b = Option Required b

data Option a b where 
  Absent :: OptionFlag a => Option a b
  Present :: OptionFlag a => b -> Option a b

op'Last :: Option a b -> Option a b -> Option a b
op'Last Absent y = y
op'Last x Absent = x
op'Last x y = y

op'Merge :: (Monoid b) => Option a b -> Option a b -> Option a b
op'Merge Absent y = y
op'Merge x Absent = x
op'Merge (Present x) (Present y) = Present (mappend x y)

instance (OptionFlag a,Show b) => Show (Option a b) where
  show Absent = "Absent"
  show (Present b) = "Present ("++show b++")"

instance (OptionFlag a,Eq b) => Eq (Option a b) where
  (==) Absent Absent = True
  (==) (Present x) (Present y) = (==) x y
  (==) _ _ = False

instance (OptionFlag a,Ord b) => Ord (Option a b) where
  compare Absent Absent = EQ
  compare Absent _ = LT
  compare _ Absent = GT
  compare (Present x) (Present y) = compare x y
