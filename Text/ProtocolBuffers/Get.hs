{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances
  , TypeSynonymInstances, PatternGuards #-}
module Get where

import Data.ByteString(ByteString,empty)
import Data.ByteString.Char8(pack)
import Data.Sequence(Seq,empty,null)

data Message = Message { number :: Int
                       , name :: Maybe ByteString -- [ default "Curry" ] in proto file
                       , children :: Seq Message
                       , parent :: Maybe Message }
  deriving (Show)

class Default a where defaultValue :: a

class Default a => MessageGetter a b c | a b -> c where
   get :: (a -> b) -> a -> c
   isSet :: (a->b) -> a -> Bool
   isSet _ _ = True -- simplifies all required base types

--When the proto file gives default values put them in this instance
instance Default Message where
    defaultValue = Message defaultValue
                           (Just (pack "Curry"))
                           defaultValue
                           defaultValue

instance Default a => Default (Maybe a) where defaultValue = Just defaultValue
instance Default (Seq a) where defaultValue = Data.Sequence.empty
instance Default ByteString where defaultValue = Data.ByteString.empty
instance Default Int where defaultValue = 0
-- etc for all base type

instance MessageGetter Message (Maybe a) a where
   get f m | Just v <- f m = v
           | Just v <- f defaultValue = v
           | otherwise = error "Impossible case of get happened"
   isSet f m = maybe False (const True) (f m)
instance MessageGetter Message (Seq a) (Seq a) where
   get = ($)
   isSet f m = not (Data.Sequence.null (f m))
instance MessageGetter Message Int Int where get = ($)
instance MessageGetter Message ByteString ByteString where get = ($)
-- etc for all base types
