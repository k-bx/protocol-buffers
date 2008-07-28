module Text.ProtocolBuffers.MyGet(Get,runGet,Result(..)
                                 ,CompGet,runCompGet,CompResult(..)
                                 ,MonadSuspend(suspend)
                                 ,getCC,getCC1) where

import qualified Prelude as P(print)
import Prelude hiding (print)

import qualified Data.ByteString as S(ByteString,concat)
import qualified Data.ByteString as S(pack,unpack)
import qualified Data.ByteString.Lazy as L(ByteString,take,drop,length,span,splitAt,toChunks,fromChunks,null)
import qualified Data.ByteString.Lazy as L(pack,unpack)
import qualified Data.Foldable as F(foldr,foldl')    -- used with Seq
import Data.Int(Int64)                               -- index type for L.ByteString
import Data.Sequence(Seq,viewl,ViewL(..),(|>))       -- used for queue in internal state

-- These are needed to implement this module
import Control.Monad(liftM,ap)                       -- instead of Functor.fmap; ap for Applicative
import Control.Monad.Identity(Identity,runIdentity)  -- Get is a transformed Identity monad
import Data.Monoid(Monoid(mempty,mappend))           -- Writer has a Monoid contraint
import Data.Function(fix)                            -- used in clever definition of getCC

import Data.Binary.Strict.Class(BinaryParser(..))

-- The InternalGet monad is an instance of all of these...
import Control.Applicative(Alternative(..),Applicative(..))
import Control.Monad.Cont.Class(MonadCont(callCC))
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg,noMsg))
import Control.Monad.Reader.Class(MonadReader(ask,local))
import Control.Monad.Writer.Class(MonadWriter(tell,listen,pass))
import Control.Monad.State.Class(MonadState(get,put))
import Control.Monad.Trans(MonadTrans(lift),MonadIO(liftIO))
import Control.Monad(MonadPlus(mzero,mplus))


-- Simple external return type
data Result a = Failed {-# UNPACK #-} !Int64 String
              | Finished {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 a
              | Partial (L.ByteString -> Result a)

-- Complex external return type
data CompResult w user m a = CFailed {-# UNPACK #-} !Int64 String
                           | CFinished {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 w user a
                           | CPartial (L.ByteString -> m (CompResult w user m a))

-- Internal type, converted to an external type before returning to caller.
data IResult w user m a = IFailed {-# UNPACK #-} !Int64 String
                        | IFinished w {-# UNPACK #-} !(S user) a
                        | IPartial (L.ByteString -> m (IResult w user m a))

instance (Show a) => Show (Result a) where
  showsPrec _ (Failed n msg) = ("(Failed "++) . shows n . (' ':) . shows msg . (")"++)
  showsPrec _ (Finished bs n a) =
    ("(CFinished ("++) 
    . shows bs . (") ("++)
    . shows n . (") ("++) 
    . shows a . ("))"++)
  showsPrec _ (Partial {}) = ("(Partial <Data.ByteString.Lazy.ByteString-> Result a)"++)

instance (Show w, Show user, Show a) => Show (CompResult w user m a) where
  showsPrec _ (CFailed n msg) = ("(CFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (CFinished bs n w user a) =
    ("(CFinished ("++) 
    . shows bs . (") ("++)
    . shows n . (") ("++) 
    . shows w . (") ("++) 
    . shows user . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (CPartial {}) = ("(CPartial <Data.ByteString.Lazy.ByteString->IO (CompResult w user m a))"++)

instance (Show user, Show a, Show w) => Show (IResult w user m a) where
  showsPrec _ (IFailed n msg) = ("(IFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (IFinished w s a) =
    ("(IFinished ("++)
    . shows w . (") ("++)
    . shows s . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (IPartial {}) = ("(IPartial <Data.ByteString.Lazy.ByteString->IO (IResult w user m a))"++)

-- Internal state type
data S user = S { current :: {-# UNPACK #-} !L.ByteString
                , consumed :: {-# UNPACK #-} !Int64
                , future :: {-# UNPACK #-} !(Seq L.ByteString)
                , userField :: user
                }
  deriving Show

-- Complex external monad type
type CompGet b r w user m = InternalGet (IResult w user m b) String r w (S user) m

-- | 'runGet' is the simple executor
runCompGet :: (Monad m,Monoid w)
            => CompGet a r w user m a
            -> r -> user -> L.ByteString
            -> m (CompResult w user m a)
runCompGet g rIn userIn bs = liftM convert (unGet g sIn scIn)
  where sIn = S bs 0 mempty userIn
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc rIn mempty s (ErrorFrame ec)
            where ec msg (S _ n _ _) = return (IFailed n msg)

        convert :: (Monad m) => IResult w user m a -> CompResult w user m a
        convert (IFailed n msg) = CFailed n msg
        convert (IFinished w (S bs n future user) a) = CFinished (mappend bs (F.foldr mappend mempty future)) n w user a
        convert (IPartial f) = CPartial (\bs -> liftM convert (f bs))

-- Simple external monad type
type Get b = CompGet b () () () Identity

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
data FrameStack b e s m = ErrorFrame (e -> s -> m b)
                        | HandlerFrame (s -> FrameStack b e s m -> e -> m b) s (FrameStack b e s m)

instance Show s => Show (FrameStack b e s m) where
  showsPrec _ (ErrorFrame {}) =(++) "(ErrorFrame <e->s->m b>"
  showsPrec _ (HandlerFrame _ s _) = ("(HandlerFrame <s->FrameStack b e s m->m b> ("++)
                                     . shows s . (") <FrameStack b e s m>)"++)

-- easier name
type SuccessContinuation b e r w s m a = (a -> r -> w -> s -> FrameStack b e s m -> m b)

-- Internal monad type
newtype InternalGet b e r w s m a = InternalGet {
      unInternalGet :: SuccessContinuation b e r w s m a
              -> r                         -- reader
              -> w                         -- log so far
              -> s                         -- state
              -> FrameStack b e s m -- error stack state
              -> m b                       -- operation
    }

-- | 'runGet' is the simple executor
runGet :: Get a a -> L.ByteString -> Result a
runGet g bs = convert (runIdentity (unGet g sIn scIn))
  where sIn = S bs 0 mempty ()
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc mempty mempty s (ErrorFrame ec)
            where ec msg (S _ n _ _) = return (IFailed n msg)

        convert :: IResult () () Identity a -> Result a
        convert (IFailed n msg) = Failed n msg
        convert (IFinished _ (S bs n future _) a) = Finished (mappend bs (F.foldr mappend mempty future)) n a
        convert (IPartial f) = Partial (\bs -> convert (runIdentity (f bs)))

-- Internal access to full internal state, used by following 
getFull :: InternalGet b e r w s m s
getFull = InternalGet $ \ sc r w s pc -> sc s r w s pc
putFull :: s -> InternalGet b e r w s m ()
putFull s = InternalGet $ \ sc r w _ pc -> sc () r w s pc

-- | check that there are at least @n@ bytes available in the input,
-- and then return all input.  This will suspend if there is too
-- little data.
ensureBytes :: Int64 -> Get b L.ByteString
ensureBytes n = do
  (S bs offset future user) <- getFull
  let consume = L.take n bs
  if n == L.length consume then return bs
     else suspend >> ensureBytes n

-- | Pull @n@ bytes from the unput, as a lazy ByteString.  This will
-- suspend if there is too little data.
getLazyBytes :: Int64 -> Get b L.ByteString
getLazyBytes n = do
  (S bs offset future user) <- getFull
  let consume = L.take n bs
  if n == L.length consume then return consume
     else suspend >> getLazyBytes n

-- | Pull @n@ bytes from the input, as a strict ByteString.  This will
-- suspend if there is too little data.
getBytes :: Int -> Get b S.ByteString
getBytes nIn = do
  (S bs offset future user) <- getFull
  let n = fromIntegral nIn
      (consume, rest) = L.splitAt n bs
  if n == L.length consume
     then do let s = S rest (offset + n) future user
             putFull s
             return (S.concat (L.toChunks consume))
     else suspend >> getBytes nIn

-- | 'suspend' is supposed to allow the execution of the monad to be
-- halted, awaiting more input.  The type signature is incredibly
-- boring, since no prompting is needed and the only way to resume is
-- to continue.
class MonadSuspend m where
  suspend :: m ()

-- The instance here is fairly specific to the stack manipluation done
-- by 'addFuture' to ('S' user) and to the packaging of the resumption
-- function in 'IResult'('IPartial').
instance (Monad m) => MonadSuspend (InternalGet (IResult w user m a) e r w (S user) m) where
    suspend = InternalGet $ (\ sc r w sIn@(S bs n future user) pcIn ->
      case viewl future of
        (bs' :< future') -> sc () r w (S (mappend bs bs') n future' user) pcIn
        EmptyL -> let f bs = let sOut = appendBS sIn bs
                                 pcOut = addFuture bs pcIn
                             in sc () r w sOut pcOut
                  in return (IPartial f))
     where appendBS (S bs n future user) bs' = S bs n (future |> bs') user
           addFuture bs (HandlerFrame catcher s pc) = HandlerFrame catcher (appendBS s bs) (addFuture bs pc)
           addFuture bs x@(ErrorFrame _) = x

-- Useful wrappers around callCC

getCC :: (MonadCont m) => m (m a)
getCC = callCC (return . fix)

getCC1 :: (MonadCont m) => a -> m ( a -> m b ,  a  )
getCC1 x = callCC $ \k -> let jump a = k (jump,a) in return (jump,x)

-- The BinaryParser instance:

instance BinaryParser (Get r) where
  skip m = do let m64 = fromIntegral m
              ensureBytes 64
              (S bs n future user) <- getFull
              putFull (S (L.drop m64 bs) (n+m64) future user)
  bytesRead = fmap (\(S _ n _ _) -> fromIntegral n) getFull
  remaining = fmap (\(S bs _ future _) -> fromIntegral $ F.foldl' (\a b -> a + L.length b)
                                                                  (L.length bs) future) getFull
  isEmpty = fmap (\(S bs _ future _) -> L.null bs && F.foldr (\a b-> L.null a && b)
                                                             True future) getFull
  spanOf f = do let loop = do (S bs n future user) <- getFull
                              let (pre,post) = L.span f bs
                              putFull (S post (n + L.length pre) future user)
                              if L.null post then fmap ((L.toChunks pre)++) (suspend >> loop)
                                else return (L.toChunks pre)
                fmap S.concat loop
{-

  getWord8 = getWord8
  getByteString = getByteString

  getWord16be = getWord16be
  getWord32be = getWord32be
  getWord64be = getWord64be

  getWord16le = getWord16le
  getWord32le = getWord32le
  getWord64le = getWord64le

  getWordhost = getWordhost
  getWord16host = getWord16host
  getWord32host = getWord32host
  getWord64host = getWord64host
-}
-- Below here are the class instances
    
instance (Monad m,Error e) => Functor (InternalGet b e r w s m) where
--fmap f m = m >>= return . f
  fmap f m = InternalGet (\sc -> unInternalGet m (sc . f))

instance (Monad m,Error e) => Monad (InternalGet b e r w s m) where
  return a = InternalGet (\sc -> sc a)
  m >>= k  = InternalGet (\sc -> unInternalGet m (\a -> unInternalGet (k a) sc))
  fail msg = throwError (strMsg msg)

instance MonadTrans (InternalGet b e r w s) where
  lift m = InternalGet (\sc r w s pc -> m >>= \a -> sc a r w s pc)

instance (MonadIO m,Error e) => MonadIO (InternalGet b e r w s m) where
  liftIO = lift . liftIO

instance (Monad m, Error e) => MonadCont (InternalGet b e r w s m) where
  callCC f = InternalGet $ \sc -> let k a = InternalGet (\_ -> sc a)
                            in unInternalGet (f k) sc

instance (Monad m,Error e) => MonadError e (InternalGet b e r w s m) where
  throwError msg = InternalGet $ \sc r w s pc ->
    case pc of ErrorFrame ec -> ec msg s
               HandlerFrame catcher s1 pc1 -> catcher s1 pc1 msg
  catchError mayFail handler = InternalGet $ \sc r w s pc ->
    let pcWithHandler = let catcher s1 pc1 e1 = unInternalGet (handler e1) sc r w s1 pc1
                        in HandlerFrame catcher s pc
    in unInternalGet mayFail sc r w s pcWithHandler

instance (Monad m, Error e, Monoid w) => MonadWriter w (InternalGet b e r w s m) where
  tell w'  = InternalGet (\sc r w -> sc () r (mappend w w'))
  listen m = InternalGet (\sc r w -> let sc' a r' w'= sc (a,w') r' (mappend w w')
                               in unInternalGet m sc' r mempty)
  pass m   = InternalGet (\sc r w s pc -> let sc' (a,f) r' w' s' pc' = sc a r' (mappend w (f w')) s' pc'
                                    in unInternalGet m sc' r mempty s pc)

instance (Monad m, Error e) => MonadReader r (InternalGet b e r w s m) where
  ask = InternalGet (\sc r -> sc r r)
  local f m = InternalGet (\sc r -> let sc' a _ = sc' a r
                              in unInternalGet m sc' (f r))
              
instance (Monad m,Error e) => MonadState user (InternalGet b e r w (S user) m) where
  get   = InternalGet (\sc r w s -> sc (userField s) r w s)
  put u = InternalGet (\sc r w s -> let s' = s {userField=u}
                              in sc () r w s')

instance (Monad m, Error e) => MonadPlus (InternalGet b e r w s m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

instance (Monad m,Error e) => Applicative (InternalGet b e r w s m) where
  pure = return
  (<*>) = ap

instance (Monad m,Error e) => Alternative (InternalGet b e r w s m) where
  empty = mzero
  (<|>) = mplus
