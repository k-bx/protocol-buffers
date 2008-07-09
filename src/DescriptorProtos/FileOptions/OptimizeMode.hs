module DescriptorProtos.FileOptions.OptimizeMode
  (OptimizeMode(..))
 where

import Data.Monoid
import ProtocolBuffers.Header

data OptimizeMode = SPEED | CODE_SIZE
  deriving (Show,Eq,Ord,Typeable)

instance OptionFlag a => Monoid (Option a OptimizeMode) where mempty = Absent; mappend = op'Last

err'Name :: String
err'Name = "DescriptorProtos.FileOptions.OptimizeMode"

err' :: String -> b
err' a = error (err'Name ++ " : " ++ show a)

instance Enum OptimizeMode where
  fromEnum SPEED = 1
  fromEnum CODE_SIZE = 2

  toEnum 1 = SPEED
  toEnum 2 = CODE_SIZE
  toEnum x = err' ("toEnum failed on value "++show x)

  succ SPEED = CODE_SIZE
  succ CODE_SIZE = err' ("succ failed on value "++show CODE_SIZE)

  pred SPEED = err' ("pred failed on value "++show SPEED)
  pred CODE_SIZE = SPEED
