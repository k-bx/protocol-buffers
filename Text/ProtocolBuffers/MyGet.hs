{-# LANGUAGE CPP #-}
module Text.ProtocolBuffers.MyGet(Get,runGet,Result(..)
                                 ,CompGet,runCompGet,CompResult(..)
                                 ,ensureBytes,suspendUntilComplete,getLazyByteString -- main primitives
                                 ,getCC,getCC1 -- too good not to include
                                  -- implementation of BinaryParser (for Int64 and Lazy bytestrings)
                                 ,skip,bytesRead,isEmpty,remaining,spanOf
                                 ,getWord8,getByteString
                                 ,getWord16be,getWord32be,getWord64be
                                 ,getWord16le,getWord32le,getWord64le
                                 ,getWordhost,getWord16host,getWord32host,getWord64host
                                 ) where

import qualified Prelude as P(print)
import Prelude hiding (print)

import Data.Bits(Bits((.|.)))
import qualified Data.ByteString as S(concat)
import qualified Data.ByteString as S(pack,unpack) -- XXX testing
import qualified Data.ByteString.Internal as S(ByteString,toForeignPtr,inlinePerformIO)
import qualified Data.ByteString.Unsafe as S(unsafeIndex)
import qualified Data.ByteString.Lazy as L(ByteString,take,drop,length,span,splitAt,toChunks,fromChunks,null)
import qualified Data.ByteString.Lazy as L(pack,unpack) -- XXX testing
import qualified Data.Foldable as F(foldr1)          -- used with Seq
import Foreign.Ptr(castPtr,plusPtr)
import Foreign.ForeignPtr(withForeignPtr)
import Data.Int(Int64)                               -- index type for L.ByteString
import Data.Sequence(Seq,null,(|>))                  -- used for queue in internal state
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Foreign.Storable(Storable(peek,sizeOf))

-- These are needed to implement this modul
import Control.Monad(liftM,ap)                       -- instead of Functor.fmap; ap for Applicative
import Control.Monad(replicateM)                      -- XXX testing
import Control.Monad.Identity(Identity,runIdentity)  -- Get is a transformed Identity monad
import Data.Monoid(Monoid(mempty,mappend))           -- Writer has a Monoid contraint
import Data.Function(fix)                            -- used in clever definition of getCC

import qualified Data.Binary.Strict.Class as P(BinaryParser(..))

-- The InternalGet monad is an instance of all of these...
import Control.Applicative(Alternative(..),Applicative(..))
import Control.Monad.Cont.Class(MonadCont(callCC))
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg,noMsg))
import Control.Monad.Reader.Class(MonadReader(ask,local))
import Control.Monad.Writer.Class(MonadWriter(tell,listen,pass))
import Control.Monad.State.Class(MonadState(get,put))
import Control.Monad.Trans(MonadTrans(lift),MonadIO(liftIO))
import Control.Monad(MonadPlus(mzero,mplus))

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base(Int(..),uncheckedShiftL#)
import GHC.Word(Word16(..),Word32(..),Word64(..),uncheckedShiftL64#)
#endif

-- Simple external return type
data Result a = Failed {-# UNPACK #-} !Int64 String
              | Finished {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 a
              | Partial (Maybe L.ByteString -> Result a)

-- Complex external return type
data CompResult w user m a = CFailed {-# UNPACK #-} !Int64 String
                           | CFinished {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 w user a
                           | CPartial (Maybe L.ByteString -> m (CompResult w user m a))

-- Internal type, converted to an external type before returning to caller.
data IResult w user m a = IFailed {-# UNPACK #-} !Int64 String
                        | IFinished w {-# UNPACK #-} !(S user) a
                        | IPartial (Maybe L.ByteString -> m (IResult w user m a))

instance (Show a) => Show (Result a) where
  showsPrec _ (Failed n msg) = ("(Failed "++) . shows n . (' ':) . shows msg . (")"++)
  showsPrec _ (Finished bs n a) =
    ("(CFinished ("++) 
    . shows bs . (") ("++)
    . shows n . (") ("++) 
    . shows a . ("))"++)
  showsPrec _ (Partial {}) = ("(Partial <Maybe Data.ByteString.Lazy.ByteString-> Result a)"++)

instance (Show w, Show user, Show a) => Show (CompResult w user m a) where
  showsPrec _ (CFailed n msg) = ("(CFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (CFinished bs n w user a) =
    ("(CFinished ("++) 
    . shows bs . (") ("++)
    . shows n . (") ("++) 
    . shows w . (") ("++) 
    . shows user . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (CPartial {}) = ("(CPartial <Maybe Data.ByteString.Lazy.ByteString->IO (CompResult w user m a))"++)

instance (Show user, Show a, Show w) => Show (IResult w user m a) where
  showsPrec _ (IFailed n msg) = ("(IFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (IFinished w s a) =
    ("(IFinished ("++)
    . shows w . (") ("++)
    . shows s . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (IPartial {}) = ("(IPartial <MaybeData.ByteString.Lazy.ByteString->IO (IResult w user m a))"++)

-- Internal state type
data S user = S { current :: {-# UNPACK #-} !L.ByteString
                , consumed :: {-# UNPACK #-} !Int64
                , userField :: user
                }
  deriving Show


-- | 'runGet' is the simple executor
runCompGet :: (Monad m,Monoid w)
            => CompGet a r w user m a
            -> r -> user -> L.ByteString
            -> m (CompResult w user m a)
runCompGet g rIn userIn bs = liftM convert (unGet g sIn scIn)
  where sIn = S bs 0 userIn
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc rIn mempty s (ErrorFrame returnError True)
            where returnError msg (S _ n _) = return (IFailed n msg)
        convert :: (Monad m) => IResult w user m a -> CompResult w user m a
        convert (IFailed n msg) = CFailed n msg
        convert (IFinished w (S bs n user) a) = CFinished bs n w user a
        convert (IPartial f) = CPartial (\bs -> liftM convert (f bs))

-- easier name
type SuccessContinuation b e r w s m a = (a -> r -> w -> s -> FrameStack b e s m -> m b)

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
data FrameStack b e s m = ErrorFrame (e -> s -> m b) Bool
                        | HandlerFrame (s -> FrameStack b e s m -> e -> m b) s (Seq L.ByteString) (FrameStack b e s m)

instance Show s => Show (FrameStack b e s m) where
  showsPrec _ (ErrorFrame _ p) =(++) "(ErrorFrame <e->s->m b> " . shows p . (")"++)
  showsPrec _ (HandlerFrame _ s future pc) = ("(HandlerFrame <s->FrameStack b e s m->e->m b> ("++)
                                     . shows s . (") ("++) . shows future . (") ("++)
                                     . shows pc . (")"++)

-- Internal monad type
newtype InternalGet b e r w s m a = InternalGet {
      unInternalGet :: SuccessContinuation b e r w s m a
              -> r                         -- reader
              -> w                         -- log so far
              -> s                         -- state
              -> FrameStack b e s m        -- error handler stack
              -> m b                       -- operation
      }

-- Complex external monad type
type CompGet b r w user m = InternalGet (IResult w user m b) String r w (S user) m

-- Simple external monad type
type Get b = CompGet b () () () Identity

-- | 'runGet' is the simple executor
runGet :: Get a a -> L.ByteString -> Result a
runGet g bs = convert (runIdentity (unGet g sIn scIn))
  where sIn = S bs 0 ()
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc mempty mempty s (ErrorFrame ec True)
            where ec msg (S _ n _) = return (IFailed n msg)

        convert :: IResult () () Identity a -> Result a
        convert (IFailed n msg) = Failed n msg
        convert (IFinished _ (S bs n _) a) = Finished bs n a
        convert (IPartial f) = Partial (\bs -> convert (runIdentity (f bs)))

-- Internal access to full internal state, used by following 
getFull :: InternalGet b e r w s m s
getFull = InternalGet $ \ sc r w s pc -> sc s r w s pc
putFull :: s -> InternalGet b e r w s m ()
putFull s = InternalGet $ \ sc r w _ pc -> sc () r w s pc

-- | Keep calling 'suspend' until Nothing is passed to the 'Partial'
-- continuation.  This ensures all the data has been loaded into the
-- state of the parser.
suspendUntilComplete :: Monad m => CompGet b r w user m ()
suspendUntilComplete = do
  continue <- suspend
  if continue then suspendUntilComplete
    else return ()

-- | Call suspend and throw and error with the provided @msg@ if
-- Nothing has been passed to the 'Partial' continuation.  Otherwise
-- return ().
suspendMsg :: Monad m => String -> CompGet b r w user m ()
suspendMsg msg = do continue <- suspend
                    if continue then return ()
                      else throwError msg

-- | check that there are at least @n@ bytes available in the input.
-- This will suspend if there is to little data.
ensureBytes :: Monad m => Int64 -> CompGet b r w user m ()
ensureBytes n = do
  (S bs offset user) <- getFull
  let consume = L.take n bs
  if n == L.length consume then return ()
     else suspendMsg "ensureBytes failed" >> ensureBytes n
{-# INLINE ensureBytes #-}

-- | Pull @n@ bytes from the unput, as a lazy ByteString.  This will
-- suspend if there is too little data.
getLazyByteString :: Monad m => Int64 -> CompGet b r w user m L.ByteString
getLazyByteString n = do
  (S bs offset user) <- getFull
  let (consume,rest) = L.splitAt n bs
  if n == L.length consume then do let s = S rest (offset + n) user
                                   putFull s
                                   return consume
     else suspendMsg "getLazyByteString failed" >> getLazyByteString n
{-# INLINE getLazyByteString #-} -- important

-- | 'suspend' is supposed to allow the execution of the monad to be
-- halted, awaiting more input.  The computation is supposed to
-- continue normally if this returns True, and is supposed to halt
-- without calling suspend again if this returns False.  All future
-- calls to suspend will return False automatically and no nothing
-- else.
--
class MonadSuspend m where
  suspend :: m Bool

-- The instance here is fairly specific to the stack manipluation done
-- by 'addFuture' to ('S' user) and to the packaging of the resumption
-- function in 'IResult'('IPartial').
instance (Monad m) => MonadSuspend (InternalGet (IResult w user m a) e r w (S user) m) where
    suspend = InternalGet $ \ sc r w sIn@(S bs n user) pcIn ->
      if checkBool pcIn -- Has Nothing ever been given to a partial continuation?
        then let f Nothing = let pcOut = rememberFalse pcIn
                             in sc False r w sIn pcOut
                 f (Just bs) = let sOut = appendBS sIn bs
                                   pcOut = addFuture bs pcIn
                               in sc True r w sOut pcOut
             in return (IPartial f)
        else sc False r w sIn pcIn  -- once Nothing has been given suspend is a no-op
     where appendBS (S bs n user) bs' = S (mappend bs bs') n user
           addFuture bs (HandlerFrame catcher s future pc) =
                         HandlerFrame catcher s (future |> bs) (addFuture bs pc)
           addFuture bs x = x
           checkBool (ErrorFrame _ b) = b
           checkBool (HandlerFrame _ _ _ pc) = checkBool pc
           rememberFalse (ErrorFrame ec _) = ErrorFrame ec False
           rememberFalse (HandlerFrame catcher s future pc) =
                          HandlerFrame catcher s future (rememberFalse pc)
          
-- Useful wrappers around callCC

getCC :: (MonadCont m) => m (m a)
getCC = callCC (return . fix)
{-# INLINE getCC #-}

getCC1 :: (MonadCont m) => a -> m ( a -> m b ,  a  )
getCC1 x = callCC $ \k -> let jump a = k (jump,a) in return (jump,x)
{-# INLINE getCC1 #-}

-- The BinaryParser instance:

-- | Discard the next @m@ bytes
skip :: Monad m => Int64 -> CompGet b r w user m ()
skip m | m <=0 = return ()
       | otherwise = do
  ensureBytes m
  (S bs n user) <- getFull
  putFull (S (L.drop m bs) (n+m) user)

-- | Return the number of 'bytesRead' so far.  Initially 0, never negative.
bytesRead :: Monad m => CompGet b r w user m Int64
bytesRead = fmap (\(S _ n _) -> n) getFull

-- | Return the number of bytes 'remaining' before the current input
-- runs out and 'suspend' might be called.
remaining :: Monad m => CompGet b r w user m Int64
remaining = do (S bs _ _) <- getFull
               return (L.length bs)

-- | Return True if the number of bytes 'remaining' is 0.  Any futher
-- attempts to read an empty parser will cal 'suspend'.
isEmpty :: Monad m => CompGet b r w user m Bool
isEmpty = do (S bs n user) <- getFull
             return (L.null bs)

spanOf :: Monad m => (Word8 -> Bool) ->  CompGet b r w user m (L.ByteString)
spanOf f = do let loop = do (S bs n user) <- getFull
                            let (pre,post) = L.span f bs
                            putFull (S post (n + L.length pre) user)
                            if L.null post
                              then fmap ((L.toChunks pre)++) $ do
                                     continue <- suspend
                                     if continue then loop
                                       else return (L.toChunks pre)
                              else return (L.toChunks pre)
              fmap L.fromChunks loop
{-# INLINE spanOf #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.  This will
-- suspend if there is too little data.  If the result spans multiple
-- lazy chunks then the result occupies a freshly allocated strict
-- bytestring, otherwise it fits in a single chunk and refers to the
-- same immutable memory block as the whole chunk.
getByteString :: Monad m => Int -> CompGet b r w user m S.ByteString
getByteString nIn = fmap (S.concat . L.toChunks) (getLazyByteString (fromIntegral nIn))
{-# INLINE getByteString #-} -- important

getWordhost :: Monad m => CompGet b r w user m Word
getWordhost = getPtr (sizeOf (undefined :: Word))
{-# INLINE getWordhost #-}

getWord8 :: Monad m => CompGet b r w user m Word8
getWord8 = getPtr 1
{-# INLINE getWord8 #-}

getWord16be,getWord16le,getWord16host :: Monad m => CompGet b r w user m Word16
getWord16be = do
    s <- getByteString 2
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w16` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 1))
{-# INLINE getWord16be #-}
getWord16le = do
    s <- getByteString 2
    return $! (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w16` 8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord16le #-}
getWord16host = getPtr (sizeOf (undefined :: Word16))
{-# INLINE getWord16host #-}

getWord32be,getWord32le,getWord32host :: Monad m => CompGet b r w user m Word32
getWord32be = do
    s <- getByteString 4
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w32` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w32` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w32`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 3) )
{-# INLINE getWord32be #-}
getWord32le = do
    s <- getByteString 4
    return $! (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w32` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w32` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w32`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord32le #-}
getWord32host = getPtr  (sizeOf (undefined :: Word32))
{-# INLINE getWord32host #-}


getWord64be,getWord64le,getWord64host :: Monad m => CompGet b r w user m Word64
getWord64be = do
    s <- getByteString 8
    return $! (fromIntegral (s `S.unsafeIndex` 0) `shiftl_w64` 56) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w64` 48) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w64` 40) .|.
              (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w64` 32) .|.
              (fromIntegral (s `S.unsafeIndex` 4) `shiftl_w64` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 5) `shiftl_w64` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 6) `shiftl_w64`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 7) )
{-# INLINE getWord64be #-}
getWord64le = do
    s <- getByteString 8
    return $! (fromIntegral (s `S.unsafeIndex` 7) `shiftl_w64` 56) .|.
              (fromIntegral (s `S.unsafeIndex` 6) `shiftl_w64` 48) .|.
              (fromIntegral (s `S.unsafeIndex` 5) `shiftl_w64` 40) .|.
              (fromIntegral (s `S.unsafeIndex` 4) `shiftl_w64` 32) .|.
              (fromIntegral (s `S.unsafeIndex` 3) `shiftl_w64` 24) .|.
              (fromIntegral (s `S.unsafeIndex` 2) `shiftl_w64` 16) .|.
              (fromIntegral (s `S.unsafeIndex` 1) `shiftl_w64`  8) .|.
              (fromIntegral (s `S.unsafeIndex` 0) )
{-# INLINE getWord64le #-}
getWord64host = getPtr  (sizeOf (undefined :: Word64))
{-# INLINE getWord64host #-}

instance Monad m => P.BinaryParser (CompGet b r w user m) where
  skip = skip . fromIntegral
  bytesRead = fmap fromIntegral bytesRead
  remaining = fmap fromIntegral remaining
  isEmpty = isEmpty
  spanOf = fmap (S.concat . L.toChunks) . spanOf

  getByteString = getByteString
  getWordhost = getWordhost
  getWord8 = getWord8

  getWord16be = getWord16be
  getWord32be = getWord32be
  getWord64be = getWord64be

  getWord16le = getWord16le
  getWord32le = getWord32le
  getWord64le = getWord64le

  getWord16host = getWord16host
  getWord32host = getWord32host
  getWord64host = getWord64host

-- Below here are the class instances
    
instance (Monad m,Error e) => Functor (InternalGet b e r w (S user) m) where
--fmap f m = m >>= return . f
  fmap f m = InternalGet (\sc -> unInternalGet m (sc . f))

instance (Monad m,Error e) => Monad (InternalGet b e r w (S user) m) where
  return a = InternalGet (\sc -> sc a)
  m >>= k  = InternalGet (\sc -> unInternalGet m (\a -> unInternalGet (k a) sc))
  fail msg = throwError (strMsg msg)

instance MonadTrans (InternalGet b e r w s) where
  lift m = InternalGet (\sc r w s pc -> m >>= \a -> sc a r w s pc)

instance (MonadIO m,Error e) => MonadIO (InternalGet b e r w (S user) m) where
  liftIO = lift . liftIO

instance (Monad m, Error e) => MonadCont (InternalGet b e r w (S user) m) where
  callCC f = InternalGet $ \sc -> let k a = InternalGet (\_ -> sc a)
                            in unInternalGet (f k) sc

instance (Monad m,Error e) => MonadError e (InternalGet b e r w (S user) m) where
  throwError msg = InternalGet $ \sc r w s pc ->
    case pc of ErrorFrame ec _ -> ec msg s
               HandlerFrame catcher s1 future pc1 -> catcher (collect s1 future) pc1 msg
   where collect :: (S user) -> Seq L.ByteString -> (S user)
         collect s@(S bs n user) future | Data.Sequence.null future = s
                                        | otherwise = S (mappend bs (F.foldr1 mappend future)) n user

  catchError mayFail handler = InternalGet $ \sc r w s pc ->
    let pcWithHandler = let catcher s1 pc1 e1 = unInternalGet (handler e1) sc r w s1 pc1
                        in HandlerFrame catcher s mempty pc
    in unInternalGet mayFail sc r w s pcWithHandler

instance (Monad m, Error e, Monoid w) => MonadWriter w (InternalGet b e r w (S user) m) where
  tell w'  = InternalGet (\sc r w -> sc () r (mappend w w'))
  listen m = InternalGet (\sc r w -> let sc' a r' w'= sc (a,w') r' (mappend w w')
                               in unInternalGet m sc' r mempty)
  pass m   = InternalGet (\sc r w s pc -> let sc' (a,f) r' w' s' pc' = sc a r' (mappend w (f w')) s' pc'
                                    in unInternalGet m sc' r mempty s pc)

instance (Monad m, Error e) => MonadReader r (InternalGet b e r w (S user) m) where
  ask = InternalGet (\sc r -> sc r r)
  local f m = InternalGet (\sc r -> let sc' a _ = sc' a r
                              in unInternalGet m sc' (f r))
              
instance (Monad m,Error e) => MonadState user (InternalGet b e r w (S user) m) where
  get   = InternalGet (\sc r w s -> sc (userField s) r w s)
  put u = InternalGet (\sc r w s -> let s' = s {userField=u}
                              in sc () r w s')

instance (Monad m, Error e) => MonadPlus (InternalGet b e r w (S user) m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

instance (Monad m,Error e) => Applicative (InternalGet b e r w (S user) m) where
  pure = return
  (<*>) = ap

instance (Monad m,Error e) => Alternative (InternalGet b e r w (S user) m) where
  empty = mzero
  (<|>) = mplus

chomp :: CompGet b () String () IO ()
chomp = getByteString 1 >>= \w -> tell (map (toEnum . fromEnum) (S.unpack w))

feed x (CPartial q) = q (Just (L.pack [x]))
feed x y = return y

test g bs = runCompGet g () () (L.pack bs)

test10 = test (mplus (replicateM 5 chomp >> mzero) (replicateM 10 chomp)) [1] >>= feed 2 >>= feed 3 >>= feed 4 >>= feed 5 >>= feed 6 >>= feed 7 >>=feed 8 >>= feed 9 >>= feed 10

-- getPtr copied from binary's Get.hs
------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: (Monad m,Storable a) => Int -> CompGet b r w user m a
getPtr n = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString n)
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

-- Unchecked shifts copied from binary's Get.hs
------------------------------------------------------------------------
------------------------------------------------------------------------
-- Unchecked shifts

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
#endif

#else
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL
#endif
