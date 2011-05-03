{-# LANGUAGE CPP,MagicHash,ScopedTypeVariables,FlexibleInstances,RankNTypes,TypeSynonymInstances,MultiParamTypeClasses #-}
-- | By Chris Kuklewicz, drawing heavily from binary and binary-strict,
-- but all the bugs are my own.
--
-- This file is under the usual BSD3 licence, copyright 2008.
--
-- This started out as an improvement to
-- "Data.Binary.Strict.IncrementalGet" with slightly better internals.
-- The simplified 'Get', 'runGet', 'Result' trio with the
-- "Data.Binary.Strict.Class.BinaryParser" instance are an _untested_
-- upgrade from IncrementalGet.  Especially untested are the
-- strictness properties.
--
-- 'Get' usefully implements Applicative and Monad, MonadError,
-- Alternative and MonadPlus.  Unhandled errors are reported along
-- with the number of bytes successfully consumed.  Effects of
-- 'suspend' and 'putAvailable' are visible after
-- fail/throwError/mzero.
--
-- Each time the parser reaches the end of the input it will return a
-- Partial wrapped continuation which requests a (Maybe
-- Lazy.ByteString).  Passing (Just bs) will append bs to the input so
-- far and continue processing.  If you pass Nothing to the
-- continuation then you are declaring that there will never be more
-- input and that the parser should never again return a partial
-- contination; it should return failure or finished.
--
-- 'suspendUntilComplete' repeatedly uses a partial continuation to
-- ask for more input until 'Nothing' is passed and then it proceeds
-- with parsing.
--
-- The 'getAvailable' command returns the lazy byte string the parser
-- has remaining before calling 'suspend'.  The 'putAvailable'
-- replaces this input and is a bit fancy: it also replaces the input
-- at the current offset for all the potential catchError/mplus
-- handlers.  This change is _not_ reverted by fail/throwError/mzero.
--
-- The three 'lookAhead' and 'lookAheadM' and 'lookAheadE' functions are
-- very similar to the ones in binary's Data.Binary.Get.
--
--
-- Add specialized high-bit-run
module Text.ProtocolBuffers.Get
    (Get,runGet,runGetAll,Result(..)
     -- main primitives
    ,ensureBytes,getStorable,getLazyByteString,suspendUntilComplete
     -- parser state manipulation
    ,getAvailable,putAvailable
     -- lookAhead capabilities
    ,lookAhead,lookAheadM,lookAheadE
     -- below is for implementation of BinaryParser (for Int64 and Lazy bytestrings)
    ,skip,bytesRead,isEmpty,isReallyEmpty,remaining,spanOf,highBitRun
    ,getWord8,getByteString
    ,getWord16be,getWord32be,getWord64be
    ,getWord16le,getWord32le,getWord64le
    ,getWordhost,getWord16host,getWord32host,getWord64host
    ) where

-- The Get monad is an instance of binary-strict's BinaryParser:
-- import qualified Data.Binary.Strict.Class as P(BinaryParser(..))
-- The Get monad is an instance of all of these library classes:
import Control.Applicative(Applicative(pure,(<*>)),Alternative(empty,(<|>)))
import Control.Monad(MonadPlus(mzero,mplus),when)
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg))
-- It can be a MonadCont, but the semantics are too broken without a ton of work.

-- implementation imports
import Control.Monad(ap)                             -- instead of Functor.fmap; ap for Applicative
--import Control.Monad(replicateM,(>=>))               -- XXX testing
import Data.Bits(Bits((.|.)))
import qualified Data.ByteString as S(concat,length,null,splitAt,findIndex)
--import qualified Data.ByteString as S(unpack) -- XXX testing
import qualified Data.ByteString.Internal as S(ByteString,toForeignPtr,inlinePerformIO)
import qualified Data.ByteString.Unsafe as S(unsafeIndex)
import qualified Data.ByteString.Lazy as L(take,drop,length,span,toChunks,fromChunks,null,findIndex)
--import qualified Data.ByteString.Lazy as L(pack) -- XXX testing
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..),chunk)
import qualified Data.Foldable as F(foldr,foldr1)    -- used with Seq
import Data.Int(Int64)                               -- index type for L.ByteString
import Data.Monoid(Monoid(mempty,mappend))           -- Writer has a Monoid contraint
import Data.Sequence(Seq,null,(|>))                  -- used for future queue in handler state
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(castPtr,plusPtr)
import Foreign.Storable(Storable(peek,sizeOf))
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base(Int(..),uncheckedShiftL#)
import GHC.Word(Word16(..),Word32(..),Word64(..),uncheckedShiftL64#)
#endif

-- Simple external return type
data Result a = Failed {-# UNPACK #-} !Int64 String
              | Finished !L.ByteString {-# UNPACK #-} !Int64 a
              | Partial (Maybe L.ByteString -> Result a)

-- Internal state type, not exposed to the user.
data S = S { top :: {-# UNPACK #-} !S.ByteString
           , current :: !L.ByteString
           , consumed :: {-# UNPACK #-} !Int64
           } deriving Show

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
--
-- The ErrorFrame is the top-level error handler setup when execution begins.
-- It starts with the Bool set to True: meaning suspend can ask for more input.
-- Once suspend get 'Nothing' in reply the Bool is set to False, which means
-- that 'suspend' should no longer ask for input -- the input is finished.
-- Why store the Bool there?  It was handy when I needed to add it.
data FrameStack b = ErrorFrame (String -> S -> Result b) -- top level handler
                               Bool -- True at start, False if Nothing passed to suspend continuation
                  | HandlerFrame (Maybe ( S -> FrameStack b -> String -> Result b ))  -- encapsulated handler
                                 S  -- stored state to pass to handler
                                 (Seq L.ByteString)  -- additional input to hass to handler
                                 (FrameStack b)  -- earlier/deeper/outer handlers

type Success b a = (a -> S -> FrameStack b -> Result b)

-- Internal monad type
newtype Get a = Get {
  unGet :: forall b.    -- the forall hides the CPS style (and prevents use of MonadCont)
           Success b a  -- main continuation
        -> S            -- parser state
        -> FrameStack b -- error handler stack
        -> Result b     -- operation
    }

-- These implement the checkponting needed to store and revive the
-- state for lookAhead.  They are fragile because the setCheckpoint
-- must preceed either useCheckpoint or clearCheckpoint but not both.
-- The FutureFrame must be the most recent handler, so the commands
-- must be in the same scope depth.  Because of these constraints, the reader
-- value 'r' does not need to be stored and can be taken from the Get
-- parameter.
--
-- IMPORTANT: Any FutureFrame at the top level(s) is discarded by throwError.
setCheckpoint,useCheckpoint,clearCheckpoint :: Get ()
setCheckpoint = Get $ \ sc s pc -> sc () s (HandlerFrame Nothing s mempty pc)
useCheckpoint = Get $ \ sc (S _ _ _) frame ->
  case frame of
    (HandlerFrame Nothing s future pc) -> let (S {top=ss, current=bs, consumed=n}) = collect s future
                                          in sc () (S ss bs n) pc
    _ -> error "Text.ProtocolBuffers.Get: Impossible useCheckpoint frame!"
clearCheckpoint = Get $ \ sc s frame ->
   case frame of
     (HandlerFrame Nothing _s _future pc) -> sc () s pc
     _ -> error "Text.ProtocolBuffers.Get: Impossible clearCheckpoint frame!"

-- | 'lookAhead' runs the @todo@ action and then rewinds only the
-- BinaryParser state.  Any new input from 'suspend' or changes from
-- 'putAvailable' are kept.  Changes to the user state (MonadState)
-- are kept.  The MonadWriter output is retained.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAhead :: Get a -> Get a
lookAhead todo = do
  setCheckpoint
  a <- todo
  useCheckpoint
  return a

-- | 'lookAheadM' runs the @todo@ action. If the action returns 'Nothing' then the 
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Just' then
-- the BinaryParser is not rewound, and lookAheadM acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadM :: Get (Maybe a) -> Get (Maybe a)
lookAheadM todo = do
  setCheckpoint
  a <- todo
  maybe useCheckpoint (const clearCheckpoint) a
  return a

-- | 'lookAheadE' runs the @todo@ action. If the action returns 'Left' then the 
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Right' then
-- the BinaryParser is not rewound, and lookAheadE acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadE :: Get (Either a b) -> Get (Either a b)
lookAheadE todo = do
  setCheckpoint
  a <- todo
  either (const useCheckpoint) (const clearCheckpoint) a
  return a

-- 'collect' is used by 'putCheckpoint' and 'throwError'
collect :: S -> Seq L.ByteString -> S
collect s@(S ss bs n) future | Data.Sequence.null future = s
                             | otherwise = S ss (mappend bs (F.foldr1 mappend future)) n

-- Put the Show instances here

instance (Show a) => Show (Result a) where
  showsPrec _ (Failed n msg) = ("(Failed "++) . shows n . (' ':) . shows msg . (")"++)
  showsPrec _ (Finished bs n a) =
    ("(CFinished ("++) 
    . shows bs . (") ("++)
    . shows n . (") ("++) 
    . shows a . ("))"++)
  showsPrec _ (Partial {}) = ("(Partial <Maybe Data.ByteString.Lazy.ByteString-> Result a)"++)

instance Show (FrameStack b) where
  showsPrec _ (ErrorFrame _ p) =(++) "(ErrorFrame <e->s->m b> " . shows p . (")"++)
  showsPrec _ (HandlerFrame _ s future pc) = ("(HandlerFrame <> ("++)
                                     . shows s . (") ("++) . shows future . (") ("++)
                                     . shows pc . (")"++)

-- | 'runGet' is the simple executor
runGet :: Get a -> L.ByteString -> Result a
runGet (Get f) bsIn = f scIn sIn (ErrorFrame ec True)
  where scIn a (S ss bs n) _pc = Finished (L.chunk ss bs) n a
        sIn = case bsIn of L.Empty -> S mempty mempty 0
                           L.Chunk ss bs -> S ss bs 0
        ec msg sOut = Failed (consumed sOut) msg

-- | 'runGetAll' is the simple executor, and will not ask for any continuation because this lazy bytestring is all the input
runGetAll :: Get a -> L.ByteString -> Result a
runGetAll (Get f) bsIn = f scIn sIn (ErrorFrame ec False)
  where scIn a (S ss bs n) _pc = Finished (L.chunk ss bs) n a
        sIn = case bsIn of L.Empty -> S mempty mempty 0
                           L.Chunk ss bs -> S ss bs 0
        ec msg sOut = Failed (consumed sOut) msg

-- | Get the input currently available to the parser.
getAvailable :: Get L.ByteString
getAvailable = Get $ \ sc s@(S ss bs _) pc -> sc (L.chunk ss bs) s pc

-- | 'putAvailable' replaces the bytestream past the current # of read
-- bytes.  This will also affect pending MonadError handler and
-- MonadPlus branches.  I think all pending branches have to have
-- fewer bytesRead than the current one.  If this is wrong then an
-- error will be thrown.
--
-- WARNING : 'putAvailable' is still untested.
putAvailable :: L.ByteString -> Get ()
putAvailable bsNew = Get $ \ sc (S _ss _bs n) pc ->
  let s' = case bsNew of
             L.Empty -> S mempty mempty n
             L.Chunk ss' bs' -> S ss' bs' n
      rebuild (HandlerFrame catcher (S ss1 bs1 n1) future pc') =
               HandlerFrame catcher sNew mempty (rebuild pc')
        where balance = n - n1
              whole | balance < 0 = error "Impossible? Cannot rebuild HandlerFrame in MyGet.putAvailable: balance is negative!"
                    | otherwise = L.take balance $ L.chunk ss1 bs1 `mappend` F.foldr mappend mempty future
              sNew | balance /= L.length whole = error "Impossible? MyGet.putAvailable.rebuild.sNew HandlerFrame assertion failed."
                   | otherwise = case mappend whole bsNew of
                                   L.Empty -> S mempty mempty n1
                                   L.Chunk ss2 bs2 -> S ss2 bs2 n1
      rebuild x@(ErrorFrame {}) = x
  in sc () s' (rebuild pc)

-- Internal access to full internal state, as helper functions
getFull :: Get S
getFull = Get $ \ sc s pc -> sc s s pc
putFull :: S -> Get ()
putFull s = Get $ \ sc _s pc -> sc () s pc

-- | Keep calling 'suspend' until Nothing is passed to the 'Partial'
-- continuation.  This ensures all the data has been loaded into the
-- state of the parser.
suspendUntilComplete :: Get ()
suspendUntilComplete = do
  continue <- suspend
  when continue suspendUntilComplete

-- | Call suspend and throw and error with the provided @msg@ if
-- Nothing has been passed to the 'Partial' continuation.  Otherwise
-- return ().
suspendMsg :: String -> Get ()
suspendMsg msg = do continue <- suspend
                    if continue then return ()
                      else throwError msg

-- | check that there are at least @n@ bytes available in the input.
-- This will suspend if there is to little data.
ensureBytes :: Int64 -> Get ()
ensureBytes n = do
  (S ss bs _read) <- getFull
  if n < fromIntegral (S.length ss)
    then return ()
    else do if n == L.length (L.take n (L.chunk ss bs))
              then return ()
              else suspendMsg "ensureBytes failed" >> ensureBytes n
{-# INLINE ensureBytes #-}

-- | Pull @n@ bytes from the unput, as a lazy ByteString.  This will
-- suspend if there is too little data.
getLazyByteString :: Int64 -> Get L.ByteString
getLazyByteString n | n<=0 = return mempty
                    | otherwise = do
  (S ss bs offset) <- getFull
  case splitAtOrDie n (L.chunk ss bs) of
    Just (consume,rest) ->do
       case rest of
         L.Empty -> putFull (S mempty mempty (offset + n))
         L.Chunk ss' bs' -> putFull (S ss' bs' (offset + n))
       return consume
    Nothing -> suspendMsg ("getLazyByteString failed with "++show (n,(S.length ss,L.length bs,offset)))  >> getLazyByteString n
{-# INLINE getLazyByteString #-} -- important

-- | 'suspend' is supposed to allow the execution of the monad to be
-- halted, awaiting more input.  The computation is supposed to
-- continue normally if this returns True, and is supposed to halt
-- without calling suspend again if this returns False.  All future
-- calls to suspend will return False automatically and no nothing
-- else.
--
-- These semantics are too specialized to let this escape this module.
class MonadSuspend m where
  suspend :: m Bool

-- The instance here is fairly specific to the stack manipluation done
-- by 'addFuture' to ('S' user) and to the packaging of the resumption
-- function in 'IResult'('IPartial').
instance MonadSuspend Get where
    suspend = Get (
-- XXX I moved checkBool, addFuture, and rememberFalse inside the Get ( ) from
-- their previous location in the where clause below (with appendBS).
--
-- XXX This is because ghc-7.0.2 had error:
{-
Text/ProtocolBuffers/Get.hs:304:15:
    Couldn't match type `b1' with `b'
      because type variable `b' would escape its scope
    This (rigid, skolem) type variable is bound by
      a type expected by the context:
        Success b Bool -> S -> FrameStack b -> Result b
    The following variables have types that mention b1
      addFuture :: L.ByteString -> FrameStack b1 -> FrameStack b1
        (bound at Text/ProtocolBuffers/Get.hs:315:12)
-}
-- XXX I am worried this may change the allocation behavior of the program.
      let checkBool (ErrorFrame _ b) = b
          checkBool (HandlerFrame _ _ _ pc) = checkBool pc
          -- addFuture puts the new data in 'future' where throwError's collect can find and use it
          addFuture bs (HandlerFrame catcher s future pc) =
                        HandlerFrame catcher s (future |> bs) (addFuture bs pc)
          addFuture _bs x@(ErrorFrame {}) = x
          -- Once suspend is given Nothing, it remembers this and always returns False
          rememberFalse (ErrorFrame ec _) = ErrorFrame ec False
          rememberFalse (HandlerFrame catcher s future pc) =
                         HandlerFrame catcher s future (rememberFalse pc)
      in \ sc sIn pcIn ->
      if checkBool pcIn -- Has Nothing ever been given to a partial continuation?
        then let f Nothing = let pcOut = rememberFalse pcIn
                             in sc False sIn pcOut
                 f (Just bs') = let sOut = appendBS sIn bs'
                                    pcOut = addFuture bs' pcIn
                                in sc True sOut pcOut
             in Partial f
        else sc False sIn pcIn  -- once Nothing has been given suspend is a no-op
                  )
     where appendBS (S ss bs n) bs' = S ss (mappend bs bs') n

-- A unique sort of command...

-- | 'discardInnerHandler' causes the most recent catchError to be
-- discarded, i.e. this reduces the stack of error handlers by removing
-- the top one.  These are the same handlers which Alternative((<|>)) and
-- MonadPlus(mplus) use.  This is useful to commit to the current branch and let
-- the garbage collector release the suspended handler and its hold on
-- the earlier input.
discardInnerHandler :: Get ()
discardInnerHandler = Get $ \ sc s pcIn ->
  let pcOut = case pcIn of ErrorFrame {} -> pcIn
                           HandlerFrame _ _ _ pc' -> pc'
  in sc () s pcOut
{-# INLINE discardInnerHandler #-}

{- Currently unused, commented out to satisfy -Wall

-- | 'discardAllHandlers' causes all catchError handler to be
-- discarded, i.e. this reduces the stack of error handlers to the top
-- level handler.  These are the same handlers which Alternative((<|>))
-- and MonadPlus(mplus) use.  This is useful to commit to the current
-- branch and let the garbage collector release the suspended handlers
-- and their hold on the earlier input.
discardAllHandlers :: Get ()
discardAllHandlers = Get $ \ sc s pcIn ->
  let base pc@(ErrorFrame {}) = pc
      base (HandlerFrame _ _ _ pc) = base pc
  in sc () s (base pcIn)
{-# INLINE discardAllHandlers #-}
-}
-- The BinaryParser instance:

-- | Discard the next @m@ bytes
skip :: Int64 -> Get ()
skip m | m <=0 = return ()
       | otherwise = do
  ensureBytes m
  (S ss bs n) <- getFull
  case L.drop m (L.chunk ss bs) of
    L.Empty -> putFull (S mempty mempty (n+m))
    L.Chunk ss' bs' -> putFull (S ss' bs' (n+m))

-- | Return the number of 'bytesRead' so far.  Initially 0, never negative.
bytesRead :: Get Int64
bytesRead = fmap consumed getFull

-- | Return the number of bytes 'remaining' before the current input
-- runs out and 'suspend' might be called.
remaining :: Get Int64
remaining = do (S ss bs _) <- getFull
               return $ fromIntegral (S.length ss) + (L.length bs)

-- | Return True if the number of bytes 'remaining' is 0.  Any futher
-- attempts to read an empty parser will call 'suspend' which might
-- result in more input to consume.
--
-- Compare with 'isReallyEmpty'
isEmpty :: Get Bool
isEmpty = do (S ss bs _n) <- getFull
             return $ (S.null ss) && (L.null bs)

-- | Return True if the input is exhausted and will never be added to.
-- Returns False if there is input left to consume.
--
-- Compare with 'isEmpty'
isReallyEmpty :: Get Bool
isReallyEmpty = do
  b <- isEmpty
  if b then loop
    else return b
 where loop = do
         continue <- suspend
         if continue
           then do b <- isEmpty
                   if b then loop
                     else return b
           else return True


-- | get the longest prefix of the input where the high bit is set as well as following byte.
-- This made getVarInt slower.
highBitRun :: Get Int64
{-# INLINE highBitRun #-}
highBitRun = loop where
  loop :: Get Int64
  {-# INLINE loop #-}
  loop = do
    (S ss bs _n) <- getFull
    let mi = S.findIndex (128>) ss
    case mi of
      Just i -> return (fromIntegral i)
      Nothing -> do
        let mj = L.findIndex (128>) bs
        case mj of
          Just j -> return (fromIntegral (S.length ss) + j)
          Nothing -> do
            continue <- suspend
            if continue then loop
              else throwError "highBitRun has failed"

-- | get the longest prefix of the input where all the bytes satisfy the predicate.
spanOf :: (Word8 -> Bool) ->  Get (L.ByteString)
spanOf f = do let loop = do (S ss bs n) <- getFull
                            let (pre,post) = L.span f (L.chunk ss bs)
                            case post of
                              L.Empty -> putFull (S mempty mempty (n + L.length pre))
                              L.Chunk ss' bs' -> putFull (S ss' bs' (n + L.length pre))
                            if L.null post
                              then do continue <- suspend
                                      if continue then  fmap ((L.toChunks pre)++) loop
                                        else return (L.toChunks pre)
                              else return (L.toChunks pre)
              fmap L.fromChunks loop
{-# INLINE spanOf #-}

-- | Pull @n@ bytes from the input, as a strict ByteString.  This will
-- suspend if there is too little data.  If the result spans multiple
-- lazy chunks then the result occupies a freshly allocated strict
-- bytestring, otherwise it fits in a single chunk and refers to the
-- same immutable memory block as the whole chunk.
getByteString :: Int -> Get S.ByteString
getByteString nIn | nIn <= 0 = return mempty
                  | otherwise = do
  (S ss bs n) <- getFull
  if nIn < S.length ss
    then do let (pre,post) = S.splitAt nIn ss
            putFull (S post bs (n+fromIntegral nIn))
            return pre
    -- Expect nIn to be less than S.length ss the vast majority of times
    -- so do not worry about doing anything fancy here.
    else fmap (S.concat . L.toChunks) (getLazyByteString (fromIntegral nIn))
{-# INLINE getByteString #-} -- important

getWordhost :: Get Word
getWordhost = getStorable
{-# INLINE getWordhost #-}

getWord8 :: Get Word8
getWord8 = getPtr 1
{-# INLINE getWord8 #-}

getWord16be,getWord16le,getWord16host :: Get Word16
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
getWord16host = getStorable
{-# INLINE getWord16host #-}

getWord32be,getWord32le,getWord32host :: Get Word32
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
getWord32host = getStorable
{-# INLINE getWord32host #-}


getWord64be,getWord64le,getWord64host :: Get Word64
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
getWord64host = getStorable
{-# INLINE getWord64host #-}

{-

-- I no longer include the binary-strict package, but if one wants it
-- here is the instance:

instance P.BinaryParser Get where
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
-}

-- Below here are the class instances
    
instance Functor Get where
  fmap f m = Get (\sc -> unGet m (sc . f))
  {-# INLINE fmap #-}

instance Monad Get where
  return a = Get (\sc -> sc a)
  {-# INLINE return #-}
  m >>= k  = Get (\sc -> unGet m (\a -> unGet (k a) sc))
  {-# INLINE (>>=) #-}
  fail = throwError . strMsg

instance MonadError String Get where
  throwError msg = Get $ \_sc  s pcIn ->
    let go (ErrorFrame ec _) = ec msg s
        go (HandlerFrame (Just catcher) s1 future pc1) = catcher (collect s1 future) pc1 msg
        go (HandlerFrame Nothing _s1 _future pc1) = go pc1
    in go pcIn

  catchError mayFail handler = Get $ \sc s pc ->
    let pcWithHandler = let catcher s1 pc1 e1 = unGet (handler e1) sc s1 pc1
                        in HandlerFrame (Just catcher) s mempty pc
        actionWithCleanup = mayFail >>= \a -> discardInnerHandler >> return a
    in unGet actionWithCleanup sc s pcWithHandler

instance MonadPlus Get where
  mzero = throwError (strMsg "[mzero:no message]")
  mplus m1 m2 = catchError m1 (const m2)

instance Applicative Get where
  pure = return
  (<*>) = ap

instance Alternative Get where
  empty = mzero
  (<|>) = mplus

-- | I use "splitAt" without tolerating too few bytes, so write a Maybe version.
-- This is the only place I invoke L.Chunk as constructor instead of pattern matching.
-- I claim that the first argument cannot be empty.
splitAtOrDie :: Int64 -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
splitAtOrDie i ps | i <= 0 = Just (L.Empty, ps)
splitAtOrDie _i L.Empty = Nothing
splitAtOrDie i (L.Chunk x xs) | i < len = let (pre,post) = S.splitAt (fromIntegral i) x
                                          in Just (L.Chunk pre L.Empty
                                                  ,L.Chunk post xs)
                              | otherwise = case splitAtOrDie (i-len) xs of
                                              Nothing -> Nothing
                                              Just (y1,y2) -> Just (L.Chunk x y1,y2)
  where len = fromIntegral (S.length x)
{-# INLINE splitAtOrDie #-}

------------------------------------------------------------------------
-- getPtr copied from binary's Get.hs

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: (Storable a) => Int -> Get a
getPtr n = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString n)
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

-- I pushed the sizeOf into here (uses ScopedTypeVariables)
getStorable :: forall a. (Storable a) => Get a
getStorable = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString (sizeOf (undefined :: a)))
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getStorable #-}

------------------------------------------------------------------------
------------------------------------------------------------------------
-- Unchecked shifts copied from binary's Get.hs

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
