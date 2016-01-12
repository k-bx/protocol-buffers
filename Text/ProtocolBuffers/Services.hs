{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Text.ProtocolBuffers.Services(
    MethodCxt
  , MethodHandler(..)

  , methodName
  , packageName
  , serviceName

  , reifyMethods
  ) where

import           Text.ProtocolBuffers.Basic
import           Text.ProtocolBuffers.Reflections hiding (methodName,
                                                   serviceName)
import           Text.ProtocolBuffers.WireMessage

import           Data.List                        (intercalate)
import           Data.Proxy
import           GHC.TypeLits

-- | A convenience type alias for constraints on methods.
type MethodCxt name req resp = ( ReflectDescriptor req
                               , ReflectDescriptor resp
                               , Wire req
                               , Wire resp
                               , KnownSymbol name
                               )

-- | The name of a method.
methodName :: forall name req resp . KnownSymbol name => Method name req resp -> String
methodName _ = name
  where fqn = symbolVal (Proxy :: Proxy name)
        name = last (split ('.' ==) fqn)

-- | The service name which the method belongs to.
serviceName :: forall name req resp. KnownSymbol name => Method name req resp -> String
serviceName _ = name
  where fqn = symbolVal (Proxy :: Proxy name)
        name = last (init (split ('.' ==) fqn))

-- | The methods package name.
packageName :: forall name req resp. KnownSymbol name => Method name req resp -> String
packageName _ = name
  where fqn = symbolVal (Proxy :: Proxy name)
        name =  intercalate "." (drop 1 (init (init (split ('.' ==) fqn))))

-- | A reified method attached with a function to execute the method.
data MethodHandler m = forall name req resp. MethodCxt name req resp =>
                       MethodHandler (Method name req resp) (req -> m resp)

-- | This class can be used to reify a service to value level.
class HasMethod (m :: * -> *) (a :: [*]) where
  type MkHandler a m
  reifyMethods :: Service a -> [MethodHandler m] ->  MkHandler a m

instance HasMethod m '[] where
  type MkHandler '[] m = [MethodHandler m]
  reifyMethods _ hx = hx

instance (HasMethod m rest, MethodCxt name req resp) => HasMethod m (Method name req resp ': rest) where
  type MkHandler (Method name req resp ': rest) m = (req -> m resp) -> MkHandler rest m
  reifyMethods _ hx = \f -> reifyMethods (Service :: Service rest) ((MethodHandler (Method :: Method name req resp) f):hx)

-- this is a convenience helper until we have a better solution.
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
  "" -> []
  s' -> w : split p s''
    where (w, s'') = break p s'
