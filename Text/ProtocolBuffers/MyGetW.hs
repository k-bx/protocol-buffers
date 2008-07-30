{-# LANGUAGE CPP,MagicHash,ScopedTypeVariables,FlexibleInstances,MultiParamTypeClasses,TypeSynonymInstances,BangPatterns #-}
--
-- By Chris Kuklewicz, drawing heavily from binary and binary-strict.
-- but all the bugs are my own.
--
-- This file is under the usual BSD3 licence, copyright 2008.
--
-- There is a sibling to this module (once called "MyGet") that does not have
-- the MonadCont instances or machinery, and hides the "b" type variable with
-- a forall.
--
-- This started out as an improvement to Data.Binary.Stric.IncrementalGet with
-- slightly better internals.  The simplified "get,runGet,Result" trio with the
-- 'Data.Binary.Strict.Class.BinaryParser' instance are an _untested_
-- upgrade from IncrementalGet.  Especially untested are the
-- strictness properties.
--
-- 'Get' implement usefully Applicative and Monad, MonadError,
-- Alternative and MonadPlus, and MonadCont.
-- 
-- The 'CompGet' monad transformer has those and also useful MonadRead,
-- MonadWriter, and MonadState implementations.  Output to the writer
-- and changes to the State are thrown away when fail/throwError/mzero
-- is called.
-- 
-- The semantics of throwError interating with MonadState and MonadWriter and BinaryParser: an error
-- reverts all changes to the state and drops all items tell'd.  The position in the input stream is
-- rewound, but all items gotten from 'suspend' are kept.  Any changes
-- from 'putAvailable' are kept.  Thus throwError aborts any 'listen' or 'pass' contexts.
--
-- The semantics of callCC with catchError: If you leave the scope of
-- a catchError handler via a continuation then it counts as a
-- success and the handler is not run.  If you capture the continuation inside the scope
-- of a catchError handler and return to this later then one of two things will happen:
--   (*) If execution has never left the scope then there is no effect on the handler or scope.
--       Thus uses of callCC and its continuation wholly inside such a scope do not interfere
--       with MonadError.
--   (*) If execution has left the scope then the scope and its handler are reinstanted but
--       with the current MonadState and BinaryParser being the new checkpoint.
-- The net result of calling a continuation is to restore the same chain of error handlers
-- as existed when callCC captured it.  The oldest scopes which are still in effect are unchanged,
-- while newer scopes are restored with new checkpoint data.
--
-- The semantics of callCC interating with MonadWriter are identical to (WriterT w (ContT r m)):
--    The writer state is captured by callCC and restored when resuming, discarding changes.
--    Any 'listen' and 'pass' contexts present when the continuation was captured are restored.
--    This avoids sorting out what to do about the aborted 'listen' and 'pass' contexts
--    at the site where the continuation was called.
-- Note that (ContT r (WriterT w m)) is NOT a MonadWriter instance in the library.
--
-- The semantics of callCC interating with MonadState and BinaryParser:
--    The state and input when a continuaton is called is kept, it is not restored to
--    the state and input present when the continuaton was captured by callCC.
--
-- Top level errors are reported along with number of bytes successfully consumed.
--
-- Each time the parser reaches the end of the input it will return a Partial
-- or CPartial wrapped continuation which requests a (Maybe Lazy.ByteString).
-- Passing (Just bs) will append bs to the input so far.  If you pass Nothing
-- to the continuation, then you are declaring that there will never be more
-- input and the parser should never again return a partial contination; it
-- should return failure or finished.
--
-- 'suspendUntilComplete' repeatedly uses a partial continuation to
-- ask for more input until Nothing is passed and then it proceeds
-- with parsing.
--
-- The 'getAvailable' command returns the lazy byte string the parser
-- has remaining before calling 'suspend'.  The 'putAvailable'
-- replaces this input and is a bit fancy: it also replaces the input
-- at the current offset for all the potential catchError/mplus
-- handlers.  This change is _not_ reverted by fail/throwError/mzero.
--
-- The three 'lookAhead' and 'lookAheadM' and 'lookAheadE' functions are
-- the same as the ones in binary's Data.Binary.Get.
--
-- 'getCC' and 'getCC1' are handy wrapper of 'callCC' that I have included here.
--
-- A useful upgrade would be to "reverse the order" of the MonadWriter
-- and MonadSuspend and allow IPartial to carry the monoid-so-far to
-- the caller.  This would allow for reporting of results-to-date when
-- suspending.  Making this work with MonadWriter's 'pass' semantics
-- is hard -- perhaps a simpler Monad should be used in place of
-- MonadWriter to yield a stream of results.
--
module Text.ProtocolBuffers.MyGetW
    (Get,runGet,Result(..)
    ,CompGet,runCompGet,CompResult(..)
     -- main primitives
    ,ensureBytes,getStorable,getLazyByteString,suspendUntilComplete
     -- lookAhead capabilities
    ,lookAhead,lookAheadM,lookAheadE
     -- too good not to include with this MonadCont
    ,getCC,getCC1
     -- below is for implementation of BinaryParser (for Int64 and Lazy bytestrings)
    ,skip,bytesRead,isEmpty,remaining,spanOf
    ,getWord8,getByteString
    ,getWord16be,getWord32be,getWord64be
    ,getWord16le,getWord32le,getWord64le
    ,getWordhost,getWord16host,getWord32host,getWord64host
    ) where

-- import Debug.Trace(trace)

-- The InternalGet monad is an instance of binary-strict's BinaryParser:
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

import Data.Char

-- implementation imports
import Control.Monad(liftM,ap)                       -- instead of Functor.fmap; ap for Applicative
import Control.Monad(replicateM)                      -- XXX testing
import Control.Monad((>=>)) -- XX testing
import Control.Monad.Identity(Identity,runIdentity)  -- Get is a transformed Identity monad
import Data.Bits(Bits((.|.)))
import qualified Data.ByteString as S(concat,length,null,splitAt)
import qualified Data.ByteString as S(pack,unpack) -- XXX testing
import qualified Data.ByteString.Internal as S(ByteString,toForeignPtr,inlinePerformIO)
import qualified Data.ByteString.Unsafe as S(unsafeIndex)
import qualified Data.ByteString.Lazy as L(take,drop,length,span,toChunks,fromChunks,null,empty)
import qualified Data.ByteString.Lazy as L(pack,unpack) -- XXX testing
import qualified Data.ByteString.Lazy.Internal as L(ByteString(..),chunk)
import qualified Data.Foldable as F(foldr,foldr1)    -- used with Seq
import Data.Function(fix)                            -- used in clever definition of getCC
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

-- easier name
type SuccessContinuation b e r w user m a = (a -> r -> w -> (S user) -> TopFrame b e w user m -> m (IResult w user m b))

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
data FrameStack b e w user m = ErrorFrame (e -> (S user) -> m (IResult w user m b)) -- top level handler
                                          Bool -- True at start, False if Nothing passed to suspend continuation
                             | HandlerFrame Word64
                                            ( (S user) -> TopFrame b e w user m
                                              -> e -> m (IResult w user m b) )  -- encapsulated handler
                                            (S user)  -- stored state to pass to handler
                                            (Seq L.ByteString)  -- additiona input to hass to handler
                                            (FrameStack b e w user m)  -- deeped/outer handlers
                             | FutureFrame Word64 (S user) (Seq L.ByteString) (FrameStack b e w user m) -- for look ahead

-- TopFrame is used as storage for a unique counter (Word64) that is
-- used to label the HanderFrames in increasing order as they are
-- created.  This is only needed so that callCC can distinguish the age
-- of items in the FrameStack as being present at the user of 
-- callCC or being newer.
--
-- TopFrame's unique value is used and incremented when a new HandlerFrame is
-- allocated by catchError.
data TopFrame b e w user m = TopFrame Word64 (FrameStack b e w user m) -- next unique number for HandlerFrame, starts at 1
  deriving Show

-- Internal monad type
newtype InternalGet b e r w user m a = InternalGet {
      unInternalGet :: SuccessContinuation b e r w user m a
              -> r                         -- reader
              -> w                         -- log so far
              -> (S user)                  -- state
              -> TopFrame b e w user m     -- error handler stack
              -> m (IResult w user m b)    -- operation
      }

-- Complex external monad type
type CompGet b r w user m = InternalGet b String r w user m

-- Simple external monad type
type Get b = CompGet b () () () Identity

-- These implement the checkponting needed to store and revive the
-- state for lookAhead.  They are fragile because the setCheckpoint
-- must preceed either useCheckpoint or clearCheckpoint but not both.
-- The FutureFrame must be the most recent handler, so the commands
-- must be in the same scope depth.  Because of these constraints, the reader
-- value 'r' does not need to be stored and can be taken from the InternalGet
-- parameter.
--
-- IMPORTANT: Any FutureFrame at the top level(s) is discarded by throwError.
setCheckpoint = InternalGet $ \ sc r w s (TopFrame u pc) -> sc () r w s (TopFrame (succ u) (FutureFrame u s mempty pc))
useCheckpoint = InternalGet $ \ sc r w (S _ _ _ user) (TopFrame u (FutureFrame _u s future pc)) ->
  let (S ss bs n _) = collect s future
  in sc () r w (S ss bs n user) (TopFrame u pc)
clearCheckpoint = InternalGet $ \ sc r w s (TopFrame u (FutureFrame _u _s _future pc)) -> sc () r w s (TopFrame u pc)

-- | 'lookAhead' runs the @todo@ action and then rewinds only the
-- BinaryParser state.  Any new input from 'suspend' or changes from
-- 'putAvailable' are kept.  Changes to the user state (MonadState)
-- are kept.  The MonadWriter output is retained.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAhead :: (Monad m, Error e) => InternalGet b e r w user m a -> InternalGet b e r w user m a
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
lookAheadM :: (Monad m, Error e) => InternalGet b e r w user m (Maybe a) -> InternalGet b e r w user m (Maybe a)
lookAheadM todo = do
  checkpoint <- setCheckpoint
  a <- todo
  maybe useCheckpoint (\_ -> clearCheckpoint) a
  return a

-- | 'lookAheadE' runs the @todo@ action. If the action returns 'Left' then the 
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Right' then
-- the BinaryParser is not rewound, and lookAheadE acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadE :: (Monad m, Error e) => InternalGet b e r w user m (Either a b) -> InternalGet b e r w user m (Either a b)
lookAheadE todo = do
  checkpoint <- setCheckpoint
  a <- todo
  either (\_ -> useCheckpoint) (\_ -> clearCheckpoint) a
  return a

-- 'collect' is used by 'putCheckpoint' and 'throwError'
collect :: (S user) -> Seq L.ByteString -> (S user)
collect s@(S ss bs n user) future | Data.Sequence.null future = s
                                  | otherwise = S ss (mappend bs (F.foldr1 mappend future)) n user

instance (Show a) => Show (Result a) where
  showsPrec _ (Failed n msg) = ("(Failed "++) . shows n . (' ':) . shows msg . (")"++)
  showsPrec _ (Finished bs n a) =
    ("(CFinished ("++) 
    . shows bs . (") "++)
    . shows n . (" ("++) 
    . shows a . ("))"++)
  showsPrec _ (Partial {}) = ("(Partial <>)"++)

instance (Show w, Show user, Show a) => Show (CompResult w user m a) where
  showsPrec _ (CFailed n msg) = ("(CFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (CFinished bs n w user a) =
    ("(CFinished ("++) 
    . shows bs . (") "++)
    . shows n . (" ("++) 
    . shows w . (") ("++) 
    . shows user . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (CPartial {}) = ("(CPartial <>)"++)

instance (Show user, Show a, Show w) => Show (IResult w user m a) where
  showsPrec _ (IFailed n msg) = ("(IFailed "++) . shows n . (' ':). shows msg . (")"++)
  showsPrec _ (IFinished w s a) =
    ("(IFinished ("++)
    . shows w . (") ("++)
    . shows s . (") ("++)
    . shows a . ("))"++)
  showsPrec _ (IPartial {}) = ("(IPartial <>)"++)

instance Show s => Show (FrameStack b e w s m) where
  showsPrec _ (ErrorFrame _ p) =(++) "(ErrorFrame <> " . shows p . (")"++)
  showsPrec _ (HandlerFrame u _ s future pc) = ("(HandlerFrame "++)
    . shows u . (" <> ("++) . shows s . (") ("++) . shows future . (")\n"++) . indent . shows pc . (")"++)
  showsPrec _ (FutureFrame u s future pc) = ("(FutureFrame "++)
    . shows u . (" ("++) . shows s . (") ("++) . shows future . (")\n"++) . indent . shows pc . (")"++)

indent :: String ->String
indent = unlines . map ("  "++) . lines

-- Internal state type, not exposed to the user.
data S user = S { top :: {-# UNPACK #-} !S.ByteString
                , current :: {-# UNPACK #-} !L.ByteString
                , consumed :: {-# UNPACK #-} !Int64
                , userField :: user
                }
  deriving Show

-- | 'runGet' is the simple executor
runCompGet :: (Monad m,Monoid w)
            => CompGet a r w user m a
            -> r -> user -> L.ByteString
            -> m (CompResult w user m a)
runCompGet g rIn userIn bsIn = liftM convert (unGet g sIn scIn)
  where sIn = case bsIn of L.Empty -> S mempty mempty 0 userIn
                           L.Chunk ss bs -> S ss bs 0 userIn
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc rIn mempty s (TopFrame 1 (ErrorFrame returnError True))
            where returnError msg s = return (IFailed (consumed s) msg)
        convert :: (Monad m) => IResult w user m a -> CompResult w user m a
        convert (IFailed n msg) = CFailed n msg
        convert (IFinished w (S ss bs n user) a) = CFinished (L.chunk ss bs) n w user a
        convert (IPartial f) = CPartial (\bs -> liftM convert (f bs))

-- | 'runGet' is the simple executor
runGet :: Get a a -> L.ByteString -> Result a
runGet g bsIn = convert (runIdentity (unGet g sIn scIn))
  where sIn = case bsIn of L.Empty -> S mempty mempty 0 ()
                           L.Chunk ss bs -> S ss bs 0 ()
        scIn a _r w sOut _pc = return (IFinished w sOut a)
        unGet (InternalGet f) s sc = f sc mempty mempty s (TopFrame 1 (ErrorFrame ec True))
            where ec msg s = return (IFailed (consumed s) msg)

        convert :: IResult () () Identity a -> Result a
        convert (IFailed n msg) = Failed n msg
        convert (IFinished _ (S ss bs n _) a) = Finished (L.chunk ss bs) n a
        convert (IPartial f) = Partial (\bs -> convert (runIdentity (f bs)))

-- | Get the input currently available to the parser.
getAvailable :: InternalGet b e r w user m L.ByteString
getAvailable = InternalGet $ \ sc r w s@(S ss bs _ _) pc -> sc (L.chunk ss bs) r w s pc

-- | 'putAvailable' replaces the bytestream past the current # of read
-- bytes.  This will also affect pending MonadError handler and
-- MonadPlus branches.  I think all pending branches have to have
-- fewer bytesRead than the current one.  If this is wrong then an
-- error will be thrown.
--
-- WARNING : 'putAvailable' is still untested.
putAvailable :: L.ByteString -> InternalGet b e r w user m ()
putAvailable bsNew = InternalGet $ \ sc r w (S _ss _bs n user) (TopFrame uIn pc) ->
  let s' = case bsNew of
             L.Empty -> S mempty mempty n user
             L.Chunk ss' bs' -> S ss' bs' n user
      rebuild (HandlerFrame u catcher (S ss1 bs1 n1 user1) future pc') =
               HandlerFrame u catcher sNew mempty (rebuild pc')
        where balance = n - n1
              whole | balance < 0 = error "Impossible? Cannot rebuild HandlerFrame in MyGet.putAvailable: balance is negative!"
                    | otherwise = L.take balance $ L.chunk ss1 bs1 `mappend` F.foldr mappend mempty future
              sNew | balance /= L.length whole = error "Impossible? MyGet.putAvailable.rebuild.sNew HandlerFrame assertion failed."
                   | otherwise = case mappend whole bsNew of
                                   L.Empty -> S mempty mempty n1 user1
                                   L.Chunk ss2 bs2 -> S ss2 bs2 n1 user1
      rebuild (FutureFrame u (S ss1 bs1 n1 user1) future pc') =
               FutureFrame u sNew mempty (rebuild pc')
        where balance = n - n1
              whole | balance < 0 = error "Impossible? Cannot rebuild FutureFrame in MyGet.putAvailable: balance is negative!"
                    | otherwise = L.take balance $ L.chunk ss1 bs1 `mappend` F.foldr mappend mempty future
              sNew | balance /= L.length whole = error "Impossible? MyGet.putAvailable.rebuild.sNew FutureFrame assertion failed."
                   | otherwise = case mappend whole bsNew of
                                   L.Empty -> S mempty mempty n1 user1
                                   L.Chunk ss2 bs2 -> S ss2 bs2 n1 user1
      rebuild x@(ErrorFrame {}) = x
  in sc () r w s' (TopFrame uIn (rebuild pc))

-- Internal access to full internal state, as helepr functions
getFull :: InternalGet b e r w user m (S user)
getFull = InternalGet $ \ sc r w s pc -> sc s r w s pc
putFull :: (S user) -> InternalGet b e r w user m ()
putFull s = InternalGet $ \ sc r w _ pc -> sc () r w s pc

-- | Keep calling 'suspend' until Nothing is passed to the 'Partial'
-- continuation.  This ensures all the data has been loaded into the
-- state of the parser.
suspendUntilComplete :: ({-Show user,-} Monad m) => CompGet b r w user m ()
suspendUntilComplete = do
  continue <- suspend
  if continue then suspendUntilComplete
    else return ()

-- | Call suspend and throw and error with the provided @msg@ if
-- Nothing has been passed to the 'Partial' continuation.  Otherwise
-- return ().
suspendMsg :: ({-Show user,-} Monad m) => String -> CompGet b r w user m ()
suspendMsg msg = do continue <- suspend
                    if continue then return ()
                      else throwError msg
{-# INLINE suspendMsg #-}

-- | check that there are at least @n@ bytes available in the input.
-- This will suspend if there is to little data.
ensureBytes :: ({-Show user,-} Monad m) => Int64 -> CompGet b r w user m ()
ensureBytes n = do
  (S ss bs offset user) <- getFull
  if n < fromIntegral (S.length ss)
    then return ()
    else do if n == L.length (L.take n (L.chunk ss bs))
              then return ()
              else suspendMsg "ensureBytes failed" >> ensureBytes n
{-# INLINE ensureBytes #-}

-- | Pull @n@ bytes from the unput, as a lazy ByteString.  This will
-- suspend if there is too little data.
getLazyByteString :: ({-Show user,-} Monad m)=> Int64 -> CompGet b r w user m L.ByteString
getLazyByteString n = do
  (S ss bs offset user) <- getFull
  case splitAtOrDie n (L.chunk ss bs) of
    Just (consume,rest) -> do case rest of
                                L.Empty -> putFull (S mempty mempty (offset + n) user)
                                L.Chunk ss' bs' -> putFull (S ss' bs' (offset + n) user)
                              return consume
    Nothing -> suspendMsg "getLazyByteString failed" >> getLazyByteString n
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
instance (({-Show user,-} Monad m)) => MonadSuspend (InternalGet b e r w user m) where
    suspend = InternalGet $ \ sc r w sIn@(S ss bs n user) pcIn@(TopFrame u pcInside) ->
      if checkBool pcInside -- Has Nothing ever been given to a partial continuation?
        then let f Nothing = let pcOut = TopFrame u (rememberFalse pcInside)
                             in sc False r w sIn pcOut
                 f (Just bs') = let sOut = appendBS sIn bs'
                                    pcOut = TopFrame u (addFuture bs' pcInside)
                                in sc True r w sOut pcOut
             in return (IPartial f)
        else sc False r w sIn pcIn  -- once Nothing has been given suspend is a no-op
     where appendBS (S ss bs n user) bs' = S ss (mappend bs bs') n user
           -- addFuture puts the new data in 'future' where throwError's collect can find and use it
           addFuture bs (HandlerFrame u catcher s future pc) =
                         HandlerFrame u catcher s (future |> bs) (addFuture bs pc)
           addFuture bs (FutureFrame u s future pc) =
                         FutureFrame u s (future |> bs) (addFuture bs pc)
           addFuture _bs x@(ErrorFrame {}) = x
           -- Once suspend is given Nothing, it remembers this and always returns False
           checkBool (ErrorFrame _ b) = b
           checkBool (HandlerFrame _ _ _ _ pc) = checkBool pc
           checkBool (FutureFrame _ _ _ pc) = checkBool pc
           rememberFalse (ErrorFrame ec _) = ErrorFrame ec False
           rememberFalse (HandlerFrame u catcher s future pc) =
                          HandlerFrame u catcher s future (rememberFalse pc)
           rememberFalse (FutureFrame u s future pc) =
                          FutureFrame u s future (rememberFalse pc)
          
-- Useful wrappers around callCC

getCC :: (MonadCont m) => m (m a)
getCC = callCC (return . fix)
{-# INLINE getCC #-}

getCC1 :: (MonadCont m) => a -> m ( a -> m b ,  a  )
getCC1 x = callCC $ \k -> let jump a = k (jump,a) in return (jump,x)
{-# INLINE getCC1 #-}

-- A unique sort of command...

-- | 'discardInnerHandler' causes the most recent catchError to be
-- discarded, i.e. this reduces the stack of error handlers by removing
-- the top one.  These are the same handlers which Alternative((<|>)) and
-- MonadPlus(mplus) use.  This is useful to commit to the current branch and let
-- the garbage collector release the suspended handler and its hold on
-- the earlier input.
discardInnerHandler :: ({-Show user,-} Monad m) => InternalGet b e r w user m ()
discardInnerHandler = InternalGet $ \ sc r w s (TopFrame u pcIn) ->
  let pcOut = case pcIn of ErrorFrame {} -> pcIn
                           HandlerFrame _ _ _ _ pc' -> pc'
                           FutureFrame _ _ _ pc' -> pc'
  in sc () r w s (TopFrame u pcOut)
{-# INLINE discardInnerHandler #-}

-- | 'discardAllHandlers' causes all catchError handler to be
-- discarded, i.e. this reduces the stack of error handlers to the top
-- level handler.  These are the same handlers which Alternative((<|>))
-- and MonadPlus(mplus) use.  This is useful to commit to the current
-- branch and let the garbage collector release the suspended handlers
-- and their hold on the earlier input.
discardAllHandlers :: ({-Show user,-} Monad m) => InternalGet b e r w user m ()
discardAllHandlers = InternalGet $ \ sc r w s (TopFrame u pcIn) ->
  let base pc@(ErrorFrame {}) = pc
      base (HandlerFrame _ _ _ _ pc) = base pc
      base (FutureFrame _ _ _ pc) = base pc
  in sc () r w s (TopFrame u (base pcIn))
{-# INLINE discardAllHandlers #-}

-- The BinaryParser instance:

-- INTERNALS :
-- the getWord*,getPtr,getStorable functions call getByteString
-- getByteString works with the first strict bytestring if the request fits
--   otherwise it call getLazyByteString and allocates a new strict bytestring for the result
-- getLazyByteString calls splitAtOrDie and
--   on Nothing it suspends and if that returns True then it tries again else throwError
--   on Just it returns the desired length lazy bytestring result and advances the parser

-- | Discard the next @m@ bytes
skip :: ({-Show user,-} Monad m) => Int64 -> CompGet b r w user m ()
skip m | m <=0 = return ()
       | otherwise = do
  ensureBytes m
  (S ss bs n user) <- getFull
  case L.drop m (L.chunk ss bs) of
    L.Empty -> putFull (S mempty mempty (n+m) user)
    L.Chunk ss' bs' -> putFull (S ss' bs' (n+m) user)

-- | Return the number of 'bytesRead' so far.  Initially 0, never negative.
bytesRead :: ({-Show user,-} Monad m) => CompGet b r w user m Int64
bytesRead = fmap consumed getFull

-- | Return the number of bytes 'remaining' before the current input
-- runs out and 'suspend' might be called.
remaining :: ({-Show user,-} Monad m) => CompGet b r w user m Int64
remaining = do (S ss bs _ _) <- getFull
               return $ fromIntegral (S.length ss) + (L.length bs)

-- | Return True if the number of bytes 'remaining' is 0.  Any futher
-- attempts to read an empty parser will cal 'suspend'.
isEmpty :: ({-Show user,-} Monad m) => CompGet b r w user m Bool
isEmpty = do (S ss bs n user) <- getFull
             return $ (S.null ss) && (L.null bs)

spanOf :: ({-Show user,-} Monad m) => (Word8 -> Bool) ->  CompGet b r w user m (L.ByteString)
spanOf f = do let loop = do (S ss bs n user) <- getFull
                            let (pre,post) = L.span f (L.chunk ss bs)
                            case post of
                              L.Empty -> putFull (S mempty mempty (n + L.length pre) user)
                              L.Chunk ss' bs' -> putFull (S ss' bs' (n + L.length pre) user)
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
getByteString :: ({-Show user,-} Monad m) => Int -> CompGet b r w user m S.ByteString
getByteString nIn = do
  (S ss bs n user) <- getFull
  if nIn < S.length ss
    then do let (pre,post) = S.splitAt nIn ss
            putFull (S post bs (n+fromIntegral nIn) user)
            return pre
    -- Expect nIn to be less than S.length ss the vast majority of times
    -- so do not worry about doing anything fancy here.
    else fmap (S.concat . L.toChunks) (getLazyByteString (fromIntegral nIn))
{-# INLINE getByteString #-} -- important

getWordhost :: ({-Show user,-} Monad m) => CompGet b r w user m Word
getWordhost = getStorable
{-# INLINE getWordhost #-}

getWord8 :: ({-Show user,-} Monad m) => CompGet b r w user m Word8
getWord8 = getPtr 1
{-# INLINE getWord8 #-}

getWord16be,getWord16le,getWord16host :: ({-Show user,-} Monad m) => CompGet b r w user m Word16
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

getWord32be,getWord32le,getWord32host :: ({-Show user,-} Monad m) => CompGet b r w user m Word32
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


getWord64be,getWord64le,getWord64host :: ({-Show user,-} Monad m) => CompGet b r w user m Word64
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

instance ({-Show user,-} Monad m) => P.BinaryParser (CompGet b r w user m) where
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
    
instance ({-Show user,-} Monad m,Error e) => Functor (InternalGet b e r w user m) where
  fmap f m = InternalGet (\sc -> unInternalGet m (sc . f))
  {-# INLINE fmap #-}

instance ({-Show user,-} Monad m,Error e) => Monad (InternalGet b e r w user m) where
  return a = InternalGet (\sc -> sc a)
  {-# INLINE return #-}
  m >>= k  = InternalGet (\sc -> unInternalGet m (\a -> unInternalGet (k a) sc))
  {-# INLINE (>>=) #-}
  fail msg = throwError (strMsg msg)

instance MonadTrans (InternalGet b e r w user) where
  lift m = InternalGet (\sc r w s pc -> m >>= \a -> sc a r w s pc)

instance ({-Show user,-} MonadIO m,Error e) => MonadIO (InternalGet b e r w user m) where
  liftIO = lift . liftIO

getPC :: InternalGet b e r w user m (TopFrame b e w user m)
getPC = InternalGet $ \ sc r w s pc -> sc pc r w s pc

putPC :: TopFrame b e w user m -> InternalGet b e r w user m (TopFrame b e w user m)
putPC pc = InternalGet $ \ sc r w s _pc -> sc pc r w s pc

--countPC :: ({-Show user,-} Monad m) => CompGet b r w user m Int
countPC :: (Monad m,Error e) => InternalGet b e r w user m Int
countPC = do
  let go (ErrorFrame {}) i = i
      go (HandlerFrame _ _ _ _ pc) i = go pc $! succ i
      go (FutureFrame _ _ _ pc) i = go pc $! succ i
  (TopFrame _ pc) <- getPC
  return (go pc 0)

--instance ({-Show user,-} Monad m, Error e) => MonadCont (InternalGet b e r w user m) where
instance (Show user, Error e) => MonadCont (InternalGet b e r w user IO) where
  callCC receiver = InternalGet $ \scCaptured rCaptured wCaptured sIn pcIn@(TopFrame _ pcCaptured) ->
    let kont a = InternalGet $ \_sc _r _w sDyn (TopFrame uDyn pcDyn) ->
          scCaptured a rCaptured wCaptured sDyn (TopFrame uDyn (rebuild sDyn pcDyn pcCaptured))
  -- Error vs any
        rebuild sNew new@(ErrorFrame {}) (ErrorFrame {}) = new  -- adopt new version of shared dynamic root
        rebuild sNew new@(HandlerFrame _ _ _ _ pcNew) old@(ErrorFrame {}) = rebuild sNew pcNew old
        rebuild sNew new@(FutureFrame _ _ _ pcNew) old@(ErrorFrame {}) = rebuild sNew pcNew old
        rebuild sNew new@(ErrorFrame {}) (HandlerFrame uOld catcherOld _ _ pcOld)
            = HandlerFrame uOld catcherOld sNew mempty (rebuild sNew new pcOld)
        rebuild sNew new@(ErrorFrame {}) (FutureFrame uOld _ _ pcOld)
            = FutureFrame uOld sNew mempty (rebuild sNew new pcOld)
  -- Future vs Handler
        rebuild sNew new@(FutureFrame uNew _ _ pcNew) old@(HandlerFrame uOld catcherOld _ _ pcOld)
            = if uNew > uOld then rebuild sNew pcNew old else HandlerFrame uOld catcherOld sNew mempty (rebuild sNew new pcOld)
        rebuild sNew new@(HandlerFrame uNew _ _ _ pcNew) old@(FutureFrame uOld _ _ pcOld) 
            = if uNew > uOld then rebuild sNew pcNew old else FutureFrame uOld sNew mempty (rebuild sNew new pcOld)
  -- Two Handlers or Futures
        rebuild sNew new@(HandlerFrame uNew _ _ _ pcNew) old@(HandlerFrame uOld catcherOld _ _ pcOld) =
          case compare uNew uOld of
            GT -> rebuild sNew pcNew old    -- discard newer frame with larger uNew
            EQ -> new  -- adopt new version of shared dynamic root
            LT -> HandlerFrame uOld catcherOld sNew mempty (rebuild sNew new pcOld)
                  -- above updates and re-instantiates the old context, and proceeds deeper
        rebuild sNew new@(FutureFrame uNew _ _ pcNew) old@(FutureFrame uOld _ _ pcOld) =
          case compare uNew uOld of
            GT -> rebuild sNew pcNew old    -- discard newer frame with larger uNew
            EQ -> new  -- adopt new version of shared dynamic root
            LT -> FutureFrame uOld sNew mempty (rebuild sNew new pcOld)
                  -- above updates and re-instantiates the old context, and proceeds deeper
    in unInternalGet (receiver kont) scCaptured rCaptured wCaptured sIn pcIn

instance ({-Show user,-} Monad m,Error e) => MonadError e (InternalGet b e r w user m) where
  throwError msg = InternalGet $ \_sc _r _w s (TopFrame u pcIn) ->
    let go (ErrorFrame ec _) = ec msg s
        go (HandlerFrame _ catcher s1 future pc1) = catcher (collect s1 future) (TopFrame u pc1) msg
        go (FutureFrame _ _ _ pc1) = go pc1
    in go pcIn

  catchError mayFail handler = InternalGet $ \scCaptured rCaptured wCaptured s (TopFrame u pc) ->
    let pcWithHandler = let catcher s1 pc1 e1 = unInternalGet (handler e1) scCaptured rCaptured wCaptured s1 pc1
                        in TopFrame (succ u) (HandlerFrame u catcher s mempty pc)
        actionWithCleanup = mayFail >>= \a -> discardInnerHandler >> return a
    in unInternalGet actionWithCleanup scCaptured rCaptured wCaptured s pcWithHandler

instance ({-Show user,-} Monad m, Error e, Monoid w) => MonadWriter w (InternalGet b e r w user m) where
  tell w'  = InternalGet (\sc r w -> sc () r (mappend w w'))
  listen m = InternalGet (\sc r w -> let sc' a r' w'= sc (a,w') r' (mappend w w')
                                     in unInternalGet m sc' r mempty)
  pass m   = InternalGet (\sc r w s pc -> let sc' (a,f) r' w' s' pc' = sc a r' (mappend w (f w')) s' pc'
                                          in unInternalGet m sc' r mempty s pc)

instance ({-Show user,-} Monad m, Error e) => MonadReader r (InternalGet b e r w user m) where
  ask = InternalGet (\sc r -> sc r r)
  local f m = InternalGet (\sc r -> let sc' a _ = sc' a r
                              in unInternalGet m sc' (f r))
              
instance ({-Show user,-} Monad m,Error e) => MonadState user (InternalGet b e r w user m) where
  get   = InternalGet (\sc r w s -> sc (userField s) r w s)
  put u = InternalGet (\sc r w s -> let s' = s {userField=u}
                              in sc () r w s')

instance ({-Show user,-} Monad m, Error e) => MonadPlus (InternalGet b e r w user m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

instance ({-Show user,-} Monad m,Error e) => Applicative (InternalGet b e r w user m) where
  pure = return
  (<*>) = ap

instance ({-Show user,-} Monad m,Error e) => Alternative (InternalGet b e r w user m) where
  empty = mzero
  (<|>) = mplus

-- | I use splitAt without tolerating too few bytes, so write a Maybe version.
-- This is the only place I invoke L.Chunk as constructor instead of pattern matching.
-- I claim that the first argument cannot be empty.
splitAtOrDie :: Int64 -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
splitAtOrDie i ps | i <= 0 = Just (L.Empty, ps)
splitAtOrDie i L.Empty = Nothing
splitAtOrDie i (L.Chunk x xs) | i < len = let (pre,post) = S.splitAt (fromIntegral i) x
                                          in Just (L.Chunk pre L.Empty
                                                  ,L.Chunk post xs)
                              | otherwise = case splitAtOrDie (i-len) xs of
                                              Nothing -> Nothing
                                              Just (y1,y2) -> Just (L.Chunk x y1,y2)
  where len = fromIntegral (S.length x)
{-# INLINE splitAtOrDie #-}

-- getPtr copied from binary's Get.hs
------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: ({-Show user,-} Monad m,Storable a) => Int -> CompGet b r w user m a
getPtr n = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString n)
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

getStorable :: forall b r w user m a. ({-Show user,-} Monad m,Storable a) => CompGet b r w user m a
getStorable = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString (sizeOf (undefined :: a)))
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getStorable #-}

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

{- TESTING -}

chomp :: CompGet b () String () IO ()
chomp = getByteString 1 >>= \w -> let string = map (toEnum . fromEnum) (S.unpack w)
                                  in tell string >> pr ("chomp "++show (S.unpack w)++" <"++string++">")

feed x (CPartial q) = q (Just (L.pack [x]))
feed x y = return y

test g bs = runCompGet g () () (L.pack bs)

test10 = test (mplus (pr "go" >> replicateM 5 chomp >> pr "die" >> mzero) (pr "reborn" >> replicateM 10 chomp)) [1] >>= feed 2 >>= feed 3 >>= feed 4 >>= feed 5 >>= feed 6 >>= feed 7 >>=feed 8 >>= feed 9 >>= feed 10

pr :: (Show a) => a -> CompGet b () String () IO ()
pr = liftIO . Prelude.print

-- Ensure the stack fixing in getCC and catchError play well together:
testDepth = test depth [] >>= feed12 >>= feedNothing where
  p s = do countPC >>= \d -> bytesRead >>= \ b -> remaining >>= \r -> pr ("stack depth",d,"bytes read",b,"bytes remaining",r,s)
           pr =<< getPC 
  depth = do
    p "begin"
    catchError (p "mayFail" >> return () >> mzero)
               (\_ -> p "handler")
    p "middle"
    (_,w) <- listen $ do
      chomp
      flip catchError (\_ -> p "handler before getCC1") $ do
        (jumpWith,i) <- getCC1 0                              -- jump into first handler from nested scope
        pr ("after jump and with "++show i)
        p "after getCC1, inside 1 handler"
        flip catchError (\_ -> p "handler after getCC1" >> mzero) $ do
          p "after getCC1, inside 2 handlers, before chomp"
          chomp
          p "after getCC1, inside 2 handlers, after chomp"
          mplus (p "in mplus left" >> if i<3 then jumpWith (succ i) else mzero) -- jump from nested scope to outer scope
                (p "in mplus right" >> mzero)
    pr ("listen",w)
    p "another middle"
    j2 <- flip catchError (\_ -> p "last handler" >> return Nothing) $ do
            chomp
            p "before inner jump2"
            (jump2,j) <- getCC1 0                        -- jump2 to inside this scope from afterwards
            p ("after inner jump2 with "++ show j)
            chomp
            if odd j then throwError "Ouch"
              else return (Just jump2)
    chomp
    p "after catchError scope of jump2"
    chomp
    maybe (pr "Nothing") (\j2->pr "jump2" >> j2 1) j2     -- jump2 back into previously left scope

    p "end"
    a <- getByteString 6
    p "got 6, now suspendUntilComplete"
    suspendUntilComplete
    return a

feed12 :: CompResult w user IO a -> IO (CompResult w user IO a)
feed12 = foldr1 (>=>) . map feeds $ [ [2*i,2*i+1]  | i <- [24..45]]
  where feeds x (CPartial q) = print ("feed1",x)  >> q (Just (L.pack x))
        feeds x y = return y

feedNothing :: CompResult w user IO a -> IO (CompResult w user IO a)
feedNothing (CPartial f) = f Nothing
feedNothing x = return x

runWCI todo = runCompGet todo () () mempty

lpl :: (MonadWriter w m, Monoid w) => m (a,w->w) -> m ((a,w),w)
lpl todo = listen . pass $ do
  ((a,f),w1) <- listen todo
  return ((a,w1),f)

flipCase a | isUpper a = toLower a
           | otherwise = toUpper a

wci = runWCI $ do
  tell "a"
  x <- listen (tell "b")
  pr ("listen",x)
  y <- lpl $ do
         tell "c"
         return ("pass",map flipCase)
  pr y
  tell "d"
  return ()

{-
("listen",((),"b"))
(("pass","c"),"C")
(CFinished (Empty) (0) ("abCd") (()) (()))
-}

wci2 = runWCI $ do
  tell "a"
  (jump,i) <- getCC1 0
  pr ("I see ",i)
  if i < 3
    then do
      x <- listen (tell "b")
      pr ("listen",x)
      y <- lpl $ do
             tell "c"
             if even i then jump (succ i) else return ()
             return ("pass",map flipCase)
      pr y
      tell "d"
      jump (succ i)
    else tell "e"
  tell "f"
  return ()

{-
("I see ",0)
("listen",((),"b"))
("I see ",1)
("listen",((),"b"))
(("pass","c"),"C")
("I see ",2)
("listen",((),"b"))
("I see ",3)
(CFinished (Empty) (0) ("aef") (()) (()))
-}

-- Test jumping into and out of the 'lpl : listen . pass .listen scope:
wci3 = runWCI $ do
  tell "a0"
  (top,j) <- getCC1 0
  pr ("top sees",j)
  x <- listen (tell ",b1")
  pr ("listen",x)
  (((msg,jump,i),w1),w2) <- lpl $ do
         tell ",c2"
         (jump,i) <- getCC1 j
         pr ("jump sees",i)
         tell ",e100"
         if even i then top (succ i) else return ()
         return (("pass",jump,i),map flipCase)
  pr ((msg,i),w1,w2)
  tell ",d3"
  if i < 3 then jump (succ i) else return ()
  tell ",f1024"
  return ()
 where tell s = Control.Monad.Writer.Class.tell s >> pr ("told "++s)
{-
Annotated run of wci3 shows WriterT w (ContT r IO) semantics:

"told a0"                 -- "a0" is part of final log
                            -- ### no final log entries from the next section
("top sees",0)
"told ,b1"
("listen",((),",b1"))
"told ,c2"
("jump sees",0)
"told ,e100"
                             -- the use of 'top' erases the b1 c2 e100 above
("top sees",1) 
"told ,b1"
("listen",((),",b1"))
"told ,c2"
("jump sees",1)
"told ,e100"
(("pass",1),",c2,e100",",C2,E100")
"told ,d3"
                              -- the use of 'jump' erases the b1 C2 E100 d3
("jump sees",2)         
"told ,e100"
                              -- the use of 'top' erases the e100
                          -- ### final log entries from here down
("top sees",3)
"told ,b1"
("listen",((),",b1"))
"told ,c2"
("jump sees",3)
"told ,e100"
(("pass",3),",c2,e100",",C2,E100")
"told ,d3"
"told ,f1024"
(CFinished (Empty) (0) ("a0,b1,C2,E100,d3,f1024") (()) (()))
-}