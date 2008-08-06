{-# LANGUAGE CPP,MagicHash,ScopedTypeVariables,FlexibleInstances,MultiParamTypeClasses,TypeSynonymInstances,BangPatterns,FunctionalDependencies,RankNTypes #-}
--
-- By Chris Kuklewicz, drawing heavily from binary and binary-strict.
-- but all the bugs are my own.
--
-- This file is under the usual BSD3 licence, copyright 2008.
--
-- There is a sibling to this module (once called "MyGetW") that does
-- have a MonadCont instances, and does not hide the "b" type variable
-- with a forall.  In fact this file is a search-and-delete version of
-- MyGetW.hs
--
-- This started out as an improvement to Data.Binary.Strict.IncrementalGet with
-- slightly better internals.  The simplified "get,runGet,Result" trio with the
-- 'Data.Binary.Strict.Class.BinaryParser' instance are an _untested_
-- upgrade from IncrementalGet.  Especially untested are the
-- strictness properties.
--
-- 'Get' implements usefully Applicative and Monad, MonadError,
-- Alternative and MonadPlus.
-- 
-- The 'CompGet' monad transformer has those and also useful MonadRead,
-- MonadWriter, and MonadState implementations.  Output to the writer
-- and changes to the State are thrown away when fail/throwError/mzero
-- is called.
-- 
-- The semantics of throwError interating with MonadState and
-- MonadWriter and BinaryParser: an error reverts all changes to the
-- state and drops all items tell'd.  The position in the input stream
-- is rewound, but all items gotten from 'suspend' are kept.  Any
-- changes from 'putAvailable' are kept.  Thus throwError aborts any
-- 'listen' or 'pass' contexts.
--
-- Top level errors are reported along with number of bytes successfully consumed.
--
-- Each time the parser reaches the end of the input it will return a
-- Partial or CPartial Left-wrapped continuation which requests a
-- (Maybe Lazy.ByteString).  Passing (Just bs) will append bs to the
-- input so far.  If you pass Nothing to the continuation, then you
-- are declaring that there will never be more input and the parser
-- should never again return a partial contination; it should return
-- failure or finished.
--
-- The newest feaature is the ability for the computation to return a
-- stream of results.  The use of 'yieldItem' cannot be rolled back,
-- no subsequenct throwError,mzero,fail,lookAhead, or use of callCC
-- will lose any yielded value.  To report yielded value at an
-- intermediate point, without asking for more input, use
-- 'flushItems'.  To get a count of items yielded and not flushed, use
-- 'pendingItems', which returns 0 immediately after 'flushItems'.
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
module Text.ProtocolBuffers.MyGet
    (Get,runGet,Result(..)
    ,CompGet,runCompGet,CompResult(..)
     -- main primitives to parse items
    ,ensureBytes,getStorable,getLazyByteString,getByteString,suspendUntilComplete
     -- observe or replace the remaining BinaryParser input
    ,getAvailable,putAvailable
     -- return a Sequence of items.
    ,yieldItem,flushItems,pendingItems
     -- lookAhead capabilities
    ,lookAhead,lookAheadM,lookAheadE
     -- below is for implementation of BinaryParser (for Int64 and Lazy bytestrings)
    ,bytesRead,isEmpty,remaining
    ,skip,spanOf
    ,getWord8,getWordhost
    ,getWord16be,getWord32be,getWord64be
    ,getWord16le,getWord32le,getWord64le
    ,getWord16host,getWord32host,getWord64host
    ) where

-- import Debug.Trace(trace)

-- The InternalGet monad is an instance of binary-strict's BinaryParser:
import qualified Data.Binary.Strict.Class as P(BinaryParser(..))
-- The InternalGet monad is an instance of all of these...
import Control.Applicative(Alternative(..),Applicative(..))
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
import qualified Data.Foldable as F(foldr1)          -- used with Seq
import Data.Function(fix)                            -- used in clever definition of getCC
import Data.Int(Int64)                               -- index type for L.ByteString
import Data.Monoid(Monoid(mempty,mappend))           -- Writer has a Monoid contraint
import Data.Sequence(Seq,length,null,(|>))           -- used for future queue in handler state
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Ptr(castPtr,plusPtr)
import Foreign.Storable(Storable(peek,sizeOf))
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base(Int(..),uncheckedShiftL#)
import GHC.Word(Word16(..),Word32(..),Word64(..),uncheckedShiftL64#)
#endif

default()

-- Simple external return type
data Result y a = Failed !(Seq y) {-# UNPACK #-} !Int64 String
                | Finished !(Seq y) {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 a
                | Partial !(Seq y) !(Either ( Result y a  )  -- use laziness
                                            ( Maybe L.ByteString -> Result y a ))

-- Complex external return type
data CompResult y w user m a =
    CFailed !(Seq y) {-# UNPACK #-} !Int64 String
  | CFinished  !(Seq y) {-# UNPACK #-} !L.ByteString {-# UNPACK #-} !Int64 w user a
  | CPartial !(Seq y) !(Either ( m (CompResult y w user m a) ) -- use laziness
                                   ( Maybe L.ByteString -> m (CompResult y w user m a) ))

-- Internal type, converted to an external type before returning to caller.
data IResult y e w user m a =
    IFailed !(Seq y) {-# UNPACK #-} !Int64 e
  | IFinished !(Seq y) w {-# UNPACK #-} !(S user) a
  | IPartial !(Seq y) !(Either ( m (IResult y e w user m a) )
                               ( Maybe L.ByteString -> m (IResult y e w user m a) ))

-- Internal state type, not exposed to the user.
data S user = S { top :: {-# UNPACK #-} !S.ByteString
                , current :: {-# UNPACK #-} !L.ByteString
                , consumed :: {-# UNPACK #-} !Int64
                , user_field :: user
                }
  deriving Show

-- easier names
type SuccessContinuation b y e r w user m a =
  (a -> r -> w -> (S user) -> TopFrame b y e w user m -> m (IResult y e w user m b))

type ErrorHandler b y e w user m =
  ( (S user) -> TopFrame b y e w user m -> e -> m (IResult y e w user m b))

-- Private Internal error handling stack type
-- This must NOT be exposed by this module
data FrameStack b y e w user m =
    ErrorFrame { top_error_handler :: ErrorHandler b y e w user m -- top level handler
               , input_continues :: {-# UNPACK #-} !Bool -- True at start, False if Nothing passed to suspend continuation
               }
  | HandlerFrame { error_handler :: !(Maybe (ErrorHandler b y e w user m))  -- encapsulated handler
                 , stored_state :: {-# UNPACK #-} !(S user)  -- stored state to pass to handler
                 , future_input :: {-# UNPACK #-} !(Seq L.ByteString)  -- additiona input to hass to handler
                 , older_frame :: !(FrameStack b y e w user m)  -- older handlers
                 }

-- TopFrame is used as storage for a unique counter (Word64) that is
-- used to label the HanderFrames in increasing order as they are
-- created.  This is only needed so that callCC can distinguish the age
-- of items in the FrameStack as being present at the original use of 
-- callCC or being newer.
--
-- TopFrame's unique value is used and incremented when a new
-- HandlerFrame is allocated by catchError.  It will start at 1.
--
-- The (Seq y) of of yielded values in the TopFrame are the yielded
-- results which are waiting to be put in the next IReturn value.
data TopFrame b y e w user m  = TopFrame (Seq y) (FrameStack b y e w user m)
  deriving Show

-- Internal monad type
newtype InternalGet y e r w user m a = InternalGet {
      unInternalGet :: forall b.
                 SuccessContinuation b y e r w user m a
              -> r                          -- reader
              -> w                          -- log so far
              -> (S user)                   -- state
              -> TopFrame b y e w user m    -- error handler stack
              -> m (IResult y e w user m b) -- operation
      }

-- Complex external monad type, but errors are String
type CompGet y r w user m = InternalGet y String r w user m

-- Simple external monad type
type Get y = CompGet y () () () Identity

-- These implement the checkponting needed to store and revive the
-- state for lookAhead.  They are fragile because the setCheckpoint
-- must preceed either useCheckpoint or clearCheckpoint but not both.
-- The FutureFrame must be the most recent handler, so the commands
-- must be in the same scope depth.  Because of these constraints, the reader
-- value 'r' does not need to be stored and can be taken from the InternalGet
-- parameter.
--
-- IMPORTANT: Any FutureFrame at the top level(s) is discarded by throwError.
setCheckpoint,useCheckpoint,clearCheckpoint :: InternalGet y e r w user m ()
setCheckpoint = InternalGet $ \ sc r w s (TopFrame ys pc) ->
  sc () r w s (TopFrame ys (HandlerFrame Nothing s mempty pc))
useCheckpoint = InternalGet $ \ sc r w sIn (TopFrame ys (HandlerFrame catcher sOld future pc)) ->
  case catcher of
    Just {} -> error "Impossible: Bad use of useCheckpoint, error_handler was Just instead of Nothing"
    Nothing -> let sOut = (collect sOld future) { user_field = user_field sIn }
               in sc () r w sOut (TopFrame ys pc)
clearCheckpoint = InternalGet $ \ sc r w s (TopFrame ys (HandlerFrame catcher _s _future pc)) ->
  case catcher of
    Just {} -> error "Impossible: Bad use of clearCheckpoint, error_handler was Just instead of Nothing"
    Nothing -> sc () r w s (TopFrame ys pc)

-- | 'lookAhead' runs the @todo@ action and then rewinds only the
-- BinaryParser state.  Any new input from 'suspend' or changes from
-- 'putAvailable' are kept.  Changes to the user state (MonadState)
-- are kept.  The MonadWriter output is retained.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAhead :: (Monad m, Error e)
          => InternalGet y e r w user m a
          -> InternalGet y e r w user m a
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
lookAheadM :: (Monad m, Error e)
           => InternalGet y e r w user m (Maybe a)
           -> InternalGet y e r w user m (Maybe a)
lookAheadM todo = do
  setCheckpoint
  a <- todo
  maybe useCheckpoint (\_ -> clearCheckpoint) a
  return a

-- | 'lookAheadE' runs the @todo@ action. If the action returns 'Left' then the 
-- BinaryParser state is rewound (as in 'lookAhead').  If the action return 'Right' then
-- the BinaryParser is not rewound, and lookAheadE acts as an identity.
--
-- If an error is thrown then the entire monad state is reset to last
-- catchError as usual.
lookAheadE :: (Monad m, Error e)
           => InternalGet y e r w user m (Either a b)
           -> InternalGet y e r w user m (Either a b)
lookAheadE todo = do
  setCheckpoint
  a <- todo
  either (\_ -> useCheckpoint) (\_ -> clearCheckpoint) a
  return a

-- 'collect' is used by 'putCheckpoint' and 'throwError'
collect :: (S user) -> Seq L.ByteString -> (S user)
collect sIn future | Data.Sequence.null future = sIn
                   | otherwise = sIn { current = mappend (current sIn) (F.foldr1 mappend future) }

instance (Show a,Show y) => Show (Result y a) where
  showsPrec _ (Failed ys n msg) = scon "Failed" $ sparen ys . sshows n . sshows msg
  showsPrec _ (Finished ys bs n a) = scon "Finished" $ sparen ys . sparen bs . sshows n . sparen a
  showsPrec _ (Partial ys c) = scon "Partial" $ sparen ys . showsEither c

instance (Show w, Show user, Show a,Show y) => Show (CompResult y w user m a) where
  showsPrec _ (CFailed ys n msg) = scon "CFailed" $ sparen ys . sshows n . sshows msg
  showsPrec _ (CFinished ys bs n w user a) = scon "CFinished" $ sparen ys . sparen bs . sshows n . sparen w . sparen user . sparen a
  showsPrec _ (CPartial ys c) = scon "CPartial" $ sparen ys . showsEither c

instance (Show user, Show a, Show w,Show y,Show e) => Show (IResult y e w user m a) where
  showsPrec _ (IFailed ys n msg) = scon "IFailed" $ sparen ys . sshows n . sshows msg
  showsPrec _ (IFinished ys w s a) = scon "CFinished" $ sparen ys . sparen w . sparen s . sparen a
  showsPrec _ (IPartial ys c) = scon "CPartial" $ sparen ys . showsEither c

instance (Show s,Show y) => Show (FrameStack b y e w s m) where
  showsPrec _ (ErrorFrame _ec p) = scon "ErrorFrame <>" $ sshows p
  showsPrec _ (HandlerFrame _catcher s future pc) = scon "(HandlerFrame " $ (" <>"++) . sparen s . sparen future . ('\n':) . indent . shows pc

scon :: String -> ShowS -> ShowS
scon c s = ("("++) . (c++) . (' ':) . s . (')':)
sparen :: (Show x) => x -> ShowS
sparen s = (" ("++) . shows s . (')':)
sshows :: (Show x) => x -> ShowS
sshows s = (' ':) . shows s 
indent :: String -> String
indent = unlines . map ("  "++) . lines
showsEither :: Either a b -> ShowS
showsEither (Left {}) = shows " (Left <>)"
showsEither (Right {}) = shows " (Right <>)"

chunk :: S user -> L.ByteString
chunk s = L.chunk (top s) (current s)

-- | 'runCompGet' is the complex executor
runCompGet :: (Monad m,Monoid w)
            => CompGet y r w user m a
            -> r -> user -> L.ByteString
            -> m (CompResult y w user m a)
runCompGet g rIn userIn bsIn = liftM convert (unInternalGet g scIn rIn mempty sIn (TopFrame mempty (ErrorFrame ecIn True)))
  where sIn = case bsIn of L.Empty -> S mempty mempty 0 userIn
                           L.Chunk ss bs -> S ss bs 0 userIn
        scIn a _r w sOut (TopFrame ys _pc) = return (IFinished ys w sOut a)
        ecIn sOut (TopFrame ys _pc) msg = return (IFailed ys (consumed sOut) msg)

        convert :: (Monad m) => IResult y String w user m a -> CompResult y w user m a
        convert (IFailed ys n msg) = CFailed ys n msg
        convert (IFinished ys w (S ss bs n user) a) = CFinished ys (L.chunk ss bs) n w user a
        convert (IPartial ys f) = CPartial ys (case f of
                                                 Left f' -> Left $ liftM convert f'
                                                 Right f' -> Right $ \bs -> liftM convert (f' bs) )

-- | 'runGet' is the simple executor
runGet :: Get y a -> L.ByteString -> Result y a
runGet g bsIn = convert (runIdentity (unInternalGet g scIn () mempty sIn (TopFrame mempty (ErrorFrame ecIn True))))
  where sIn = case bsIn of L.Empty -> S mempty mempty 0 ()
                           L.Chunk ss bs -> S ss bs 0 ()
        scIn a _r w sOut (TopFrame ys _pc) = return (IFinished ys w sOut a)
        ecIn sOut (TopFrame ys _pc) msg = return (IFailed ys (consumed sOut) msg)

        convert :: IResult y String () () Identity a -> Result y a
        convert (IFailed ys n msg) = Failed ys n msg
        convert (IFinished ys _w (S ss bs n _user) a) = Finished ys (L.chunk ss bs) n a
        convert (IPartial ys f) = Partial ys (case f of
                                                Left f' -> Left $ convert (runIdentity f')
                                                Right f' -> Right $ \bs -> convert (runIdentity (f' bs)))

-- | Get the input currently available to the parser.  If this is
-- preceded by 'suspendUntilComplete' then this will definitely by the
-- complete length.
getAvailable :: InternalGet y e r w user m L.ByteString
getAvailable = InternalGet $ \ sc r w s pc -> sc (chunk s) r w s pc

-- | 'putAvailable' replaces the bytestream past the current # of read
-- bytes.  This will also affect pending MonadError handler and
-- MonadPlus branches.  I think all pending branches have to have
-- fewer bytesRead than the current one.  If this is wrong then an
-- error will be thrown.
--
-- Currently this does not change the flag which indicates whether
-- Nothing has ever been passed to a continuation to indicate the hard
-- end of input.  This poperty is subject to change.
--
-- WARNING : 'putAvailable' is still untested.
putAvailable :: L.ByteString -> InternalGet y e r w user m ()
putAvailable bsNew = InternalGet $ \ sc r w (S _ss _bs n user) (TopFrame ys pc) ->
  let s' = case bsNew of
             L.Empty -> S mempty mempty n user
             L.Chunk ss' bs' -> S ss' bs' n user
      rebuild (HandlerFrame catcher sOld@(S _ss1 _bs1 n1 user1) future pc') =
               HandlerFrame catcher sNew mempty (rebuild pc')
        where balance = n - n1
              whole | balance < 0 = error "Impossible? Cannot rebuild HandlerFrame in MyGetW.putAvailable: balance is negative!"
                    | otherwise = L.take balance . chunk $ collect sOld future
              sNew | balance /= L.length whole = error "Impossible? MyGetW.putAvailable.rebuild.sNew HandlerFrame assertion failed."
                   | otherwise = case mappend whole bsNew of
                                   L.Empty -> S mempty mempty n1 user1
                                   L.Chunk ss2 bs2 -> S ss2 bs2 n1 user1
      rebuild x@(ErrorFrame {}) = x
  in sc () r w s' (TopFrame ys (rebuild pc))

-- Internal access to full internal state, as helepr functions
getFull :: InternalGet y e r w user m (S user)
getFull = InternalGet $ \ sc r w s pc -> sc s r w s pc
putFull :: (S user) -> InternalGet y e r w user m ()
putFull s = InternalGet $ \ sc r w _s pc -> sc () r w s pc

-- | Keep calling 'suspend' until Nothing is passed to the 'Partial'
-- continuation.  This ensures all the data has been loaded into the
-- state of the parser.
suspendUntilComplete :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m ()
suspendUntilComplete = do
  continue <- suspend
  if continue then suspendUntilComplete
    else return ()

-- | Call suspend and throw and error with the provided @msg@ if
-- Nothing has been passed to the 'Partial' continuation.  Otherwise
-- return ().
suspendMsg :: ({-Show user,-} Error e, Monad m) => e -> InternalGet y e r w user m ()
suspendMsg msg = do continue <- suspend
                    if continue then return ()
                      else throwError msg
{-# INLINE suspendMsg #-}

-- | check that there are at least @n@ bytes available in the input.
-- This will suspend if there is to little data.
ensureBytes :: ({-Show user,-} Error e, Monad m) => Int64 -> InternalGet y e r w user m ()
ensureBytes n = do
  (S ss bs _offset _user) <- getFull
  if n < fromIntegral (S.length ss)
    then return ()
    else do if n == L.length (L.take n (L.chunk ss bs))
              then return ()
              else suspendMsg (strMsg "ensureBytes failed") >> ensureBytes n
{-# INLINE ensureBytes #-}

-- | Pull @n@ bytes from the unput, as a lazy ByteString.  This will
-- suspend if there is too little data.  
getLazyByteString :: ({-Show user,-} Error e, Monad m) => Int64 -> InternalGet y e r w user m L.ByteString
getLazyByteString n = do
  (S ss bs offset user) <- getFull
  case splitAtOrDie n (L.chunk ss bs) of
    Just (consume,rest) -> do case rest of
                                L.Empty -> putFull (S mempty mempty (offset + n) user)
                                L.Chunk ss' bs' -> putFull (S ss' bs' (offset + n) user)
                              return consume
    Nothing -> suspendMsg (strMsg "getLazyByteString failed") >> getLazyByteString n
{-# INLINE getLazyByteString #-} -- important

-- | 'suspend' is supposed to allow the execution of the monad to be
-- halted, awaiting more input.  The computation is supposed to
-- continue normally if this returns True, and is supposed to halt
-- without calling suspend again if this returns False.  All future
-- calls to suspend will return False automatically and no nothing
-- else.
--
-- These semantics are too specialized to let this escape this module.
class MonadSuspend y m | m -> y where
  suspend :: m Bool
  yieldItem :: y -> m ()
  flushItems :: m ()
  pendingItems :: m Int

-- The instance here is fairly specific to the stack manipluation done
-- by 'addFuture' to ('S' user) and to the packaging of the resumption
-- function in 'IResult'('IPartial').
instance ({-Show user,-} Error e, Monad m) => MonadSuspend y (InternalGet y e r w user m) where
  suspend = InternalGet $ \ sc r w sIn pcIn@(TopFrame ys pcInside) ->
    if checkBool pcInside -- Has Nothing ever been given to a partial continuation?
      then let f Nothing = let pcOut = TopFrame mempty (rememberFalse pcInside)
                           in sc False r w sIn pcOut
               f (Just bs') | L.null bs' = let pcOut = TopFrame mempty pcInside
                                           in sc True r w sIn pcOut
                            | otherwise = let sOut = appendBS sIn bs'
                                              pcOut = TopFrame mempty (addFuture bs' pcInside)
                                          in sc True r w sOut pcOut
           in return (IPartial ys (Right f))
      else sc False r w sIn pcIn  -- once Nothing has been given suspend is a no-op
   where appendBS s@(S {current=bs}) bs' = s { current=mappend bs bs' }
         -- addFuture puts the new data in 'future' where throwError's collect can find and use it
         addFuture _bs x@(ErrorFrame {}) = x
         addFuture bs x@(HandlerFrame {future_input=future, older_frame=pc}) =
           x { future_input=future |> bs, older_frame= addFuture bs pc }
         -- Once suspend is given Nothing, it remembers this and always returns False
         checkBool (HandlerFrame {older_frame=pc}) = checkBool pc
         checkBool (ErrorFrame {input_continues=b}) = b
         rememberFalse x@(ErrorFrame {}) = x { input_continues=False }
         rememberFalse x@(HandlerFrame {older_frame=pc}) = x { older_frame=rememberFalse pc }

  yieldItem y = InternalGet $ \sc r w s (TopFrame ys pc) ->
                  let ys' = ys |> y
                  in sc () r w s (TopFrame ys' pc)

  -- This does nothing when there are no items pending, otherwise it
  -- returns IPartial (Seq y) (Left ...)
  flushItems = InternalGet $ \sc r w s tf@(TopFrame ys pc) ->
                 if Data.Sequence.null ys then sc () r w s tf
                   else let lazy'rest = sc () r w s (TopFrame mempty pc)
                        in return (IPartial ys (Left lazy'rest))

  pendingItems = InternalGet $ \sc r w s pc@(TopFrame ys _pc) -> sc (Data.Sequence.length ys) r w s pc

          
-- A unique sort of command...

-- | 'discardInnerHandler' causes the most recent catchError to be
-- discarded, i.e. this reduces the stack of error handlers by removing
-- the top one.  These are the same handlers which Alternative((<|>)) and
-- MonadPlus(mplus) use.  This is useful to commit to the current branch and let
-- the garbage collector release the suspended handler and its hold on
-- the earlier input.
discardInnerHandler :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m ()
discardInnerHandler = InternalGet $ \ sc r w s (TopFrame ys pcIn) ->
  let pcOut = case pcIn of ErrorFrame {} -> pcIn
                           HandlerFrame _catcher _s _future pc' -> pc'
  in sc () r w s (TopFrame ys pcOut)
{-# INLINE discardInnerHandler #-}

-- The BinaryParser instance:

-- INTERNALS :
-- the getWord*,getPtr,getStorable functions call getByteString
-- getByteString works with the first strict bytestring if the request fits
--   otherwise it call getLazyByteString and allocates a new strict bytestring for the result
-- getLazyByteString calls splitAtOrDie and
--   on Nothing it suspends and if that returns True then it tries again else throwError
--   on Just it returns the desired length lazy bytestring result and advances the parser

-- | Discard the next @m@ bytes
skip :: ({-Show user,-} Error e, Monad m) => Int64 -> InternalGet y e r w user m ()
skip m | m <=0 = return ()
       | otherwise = do
  ensureBytes m
  (S ss bs n user) <- getFull
  case L.drop m (L.chunk ss bs) of
    L.Empty -> putFull (S mempty mempty (n+m) user)
    L.Chunk ss' bs' -> putFull (S ss' bs' (n+m) user)

-- | Return the number of 'bytesRead' so far.  Initially 0, never negative.
bytesRead :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Int64
bytesRead = fmap consumed getFull

-- | 'remaining' returns the number of bytes available before the
-- current input runs out and 'suspend' might be called (this is the
-- length of the lazy bytestring returned by 'getAvailable').  If this
-- is preceded by 'suspendUntilComplete' then this will definitely by
-- the complete length.
remaining :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Int64
remaining = do (S ss bs _n _user) <- getFull
               return $ fromIntegral (S.length ss) + (L.length bs)

-- | Return True if the number of bytes 'remaining' is 0.  Any futher
-- attempts to read an empty parser will cal 'suspend'.
isEmpty :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Bool
isEmpty = do (S ss bs _n _user) <- getFull
             return $ (S.null ss) && (L.null bs)

spanOf :: ({-Show user,-} Error e, Monad m) => (Word8 -> Bool) ->  InternalGet y e r w user m (L.ByteString)
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
getByteString :: ({-Show user,-} Error e, Monad m) => Int -> InternalGet y e r w user m S.ByteString
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

getWordhost :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Word
getWordhost = getStorable
{-# INLINE getWordhost #-}

getWord8 :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Word8
getWord8 = getPtr 1
{-# INLINE getWord8 #-}

getWord16be,getWord16le,getWord16host :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Word16
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

getWord32be,getWord32le,getWord32host :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Word32
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


getWord64be,getWord64le,getWord64host :: ({-Show user,-} Error e, Monad m) => InternalGet y e r w user m Word64
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

instance ({-Show user,-} Error e, Monad m) => P.BinaryParser (InternalGet y e r w user m) where
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
    
instance ({-Show user,-} Error e, Monad m) => Functor (InternalGet y e r w user m) where
  fmap f m = InternalGet (\sc -> unInternalGet m (sc . f))
  {-# INLINE fmap #-}

instance ({-Show user,-} Monad m,Error e) => Monad (InternalGet y e r w user m) where
  return a = InternalGet (\sc -> sc a)
  {-# INLINE return #-}
  m >>= k  = InternalGet (\sc -> unInternalGet m (\a -> unInternalGet (k a) sc))
  {-# INLINE (>>=) #-}
  fail msg = throwError (strMsg msg)

instance MonadTrans (InternalGet y e r w user) where
  lift m = InternalGet (\sc r w s pc -> m >>= \a -> sc a r w s pc)

instance ({-Show user,-} MonadIO m,Error e) => MonadIO (InternalGet y e r w user m) where
  liftIO = lift . liftIO

instance ({-Show user,-} Monad m,Error e) => MonadError e (InternalGet y e r w user m) where
  throwError msg = InternalGet $ \_sc _r _w s tf@(TopFrame ys pcIn) ->
    let go (ErrorFrame ec _b) = ec s tf msg
        go (HandlerFrame (Just catcher) s1 future pc1) = catcher (collect s1 future) (TopFrame ys pc1) msg
        go (HandlerFrame Nothing _s _future pc1) = go pc1
    in go pcIn

  catchError mayFail handler = InternalGet $ \scCaptured rCaptured wCaptured s (TopFrame ys pc) ->
    let pcWithHandler = let catcher s1 pc1 e1 = unInternalGet (handler e1) scCaptured rCaptured wCaptured s1 pc1
                        in TopFrame ys (HandlerFrame (Just catcher) s mempty pc)
        actionWithCleanup = mayFail >>= \a -> discardInnerHandler >> return a
    in unInternalGet actionWithCleanup scCaptured rCaptured wCaptured s pcWithHandler

instance ({-Show user,-} Monad m, Error e, Monoid w) => MonadWriter w (InternalGet y e r w user m) where
  tell w'  = InternalGet (\sc r w -> sc () r (mappend w w'))
  listen m = InternalGet (\sc r w -> let sc' a r' w'= sc (a,w') r' (mappend w w')
                                     in unInternalGet m sc' r mempty)
  pass m   = InternalGet (\sc r w s pc -> let sc' (a,f) r' w' s' pc' = sc a r' (mappend w (f w')) s' pc'
                                          in unInternalGet m sc' r mempty s pc)

instance ({-Show user,-} Monad m, Error e) => MonadReader r (InternalGet y e r w user m) where
  ask = InternalGet (\sc r -> sc r r)
  local f m = InternalGet (\sc r -> let sc' a _r = sc a r
                              in unInternalGet m sc' (f r))
              
instance ({-Show user,-} Monad m,Error e) => MonadState user (InternalGet y e r w user m) where
  get   = InternalGet (\sc r w s -> sc (user_field s) r w s)
  put u = InternalGet (\sc r w s -> let s' = s {user_field=u}
                              in sc () r w s')

instance ({-Show user,-} Monad m, Error e) => MonadPlus (InternalGet y e r w user m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

instance ({-Show user,-} Monad m,Error e) => Applicative (InternalGet y e r w user m) where
  pure = return
  (<*>) = ap

instance ({-Show user,-} Monad m,Error e) => Alternative (InternalGet y e r w user m) where
  empty = mzero
  (<|>) = mplus

-- | I use splitAt without tolerating too few bytes, so write a Maybe version.
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

-- getPtr copied from binary's Get.hs
------------------------------------------------------------------------
-- Primtives

-- helper, get a raw Ptr onto a strict ByteString copied out of the
-- underlying lazy byteString. So many indirections from the raw parser
-- state that my head hurts...

getPtr :: ({-Show user,-} Error e, Monad m,Storable a) => Int -> InternalGet y e r w user m a
getPtr n = do
    (fp,o,_) <- fmap S.toForeignPtr (getByteString n)
    return . S.inlinePerformIO $ withForeignPtr fp $ \p -> peek (castPtr $ p `plusPtr` o)
{-# INLINE getPtr #-}

getStorable :: forall y e r w user m a. ({-Show user,-} Error e, Monad m,Storable a) => InternalGet y e r w user m a
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


------------------------------------------------------------------------
{- TESTING -}
------------------------------------------------------------------------

chomp :: CompGet y () String () IO ()
chomp = getByteString 1 >>= \w -> tell (map (toEnum . fromEnum) (S.unpack w))

feed :: (Monad t) => Word8 -> CompResult y t1 t2 t t3 -> t (CompResult y t1 t2 t t3)
feed x (CPartial _ys (Left q)) = q
feed x (CPartial _ys (Right q)) = q (Just (L.pack [x]))
feed _x y = return y

test :: (Monoid w, Monad m) => CompGet y () w () m a -> [Word8] -> m (CompResult y w () m a)
test g bs = runCompGet g () () (L.pack bs)

test10 :: IO (CompResult () String () IO [()])
test10 = test (mplus (pr "go" >> replicateM 5 chomp >> pr "die" >> mzero) (pr "reborn" >> replicateM 10 chomp)) [1] >>= feed 2 >>= feed 3 >>= feed 4 >>= feed 5 >>= feed 6 >>= feed 7 >>=feed 8 >>= feed 9 >>= feed 10

pr :: (MonadIO m, Show a) => a -> m ()
pr = liftIO . Prelude.print

countPC :: ({-Show user,-} Monad m) => CompGet y r w user m Int
countPC = InternalGet $ \ sc r w s (TopFrame ys pc) ->
  let go (ErrorFrame {}) i = i
      go (HandlerFrame _ _ _ pc') i = go pc' $! succ i
  in sc (go pc 0) r w s (TopFrame ys pc)

{- testDepth result on my machine:

*Text.ProtocolBuffers.MyGet> testDepth
("stack depth",0,"bytes read",0,"bytes remaining",0,"begin")
("feed1",[48,49])
("stack depth",1,"bytes read",1,"bytes remaining",1,"mayFail")
("stack depth",2,"bytes read",1,"bytes remaining",1,"depth2")
("feed1",[50,51])
("stack depth",2,"bytes read",4,"bytes remaining",0,"about to mzero")
("stack depth",1,"bytes read",1,"bytes remaining",3,"middle")
("stack depth",1,"bytes read",2,"bytes remaining",2,"about to mzero again")
("stack depth",0,"bytes read",1,"bytes remaining",3,"handler")
("feed1",[52,53])
("feed1",[54,55])
("stack depth",0,"bytes read",7,"bytes remaining",1,"got 6, now suspendUntilComplete")
("feed1",[56,57])
("feed1",[58,59])
("feed1",[60,61])
("stack depth",0,"bytes read",7,"bytes remaining",7,"end")
(CFinished (Chunk "7" (Chunk "89" (Chunk ":;" (Chunk "<=" Empty)))) 7 ("0") (()) ("123456"))

The first chomp tell's "0".
All other tell's are thrown away by the error handling.
The stack depth returns to 0 as it should.
The "bytes read" is reset along with the input on each throwError/mzero/fail.
The (getByteString 6) reads "123456", leaving the "7" chunk on the input.
suspendUntilComplete loads the rest of the "89" ":;" and "<=" chunks.

-}

-- Ensure the stack fixing in catchError play words well:
testDepth :: IO (CompResult () String () IO S.ByteString)
testDepth = test depth [] >>= feed12 >>= feedNothing where
  p s = countPC >>= \d -> bytesRead >>= \ b -> remaining >>= \r ->
         pr ("stack depth",d,"bytes read",b,"bytes remaining",r,s)
  depth = do
    p "begin"
    chomp
    catchError ( p "mayFail" >>
                 ((p "depth2" >> replicateM 3 chomp >> p "about to mzero" >> mzero) <|> return ()) >>
                 p "middle" >>
                 chomp >> p "about to mzero again" >> mzero)
               (\_ -> p "handler")
    a <- getByteString 6
    p "got 6, now suspendUntilComplete"
    suspendUntilComplete
    p "end"
    return a

feed12 :: CompResult y w user IO a -> IO (CompResult y w user IO a)
feed12 = foldr1 (>=>) . map feeds $ [ [2*i,2*i+1]  | i <- [24..30]]
  where feeds x (CPartial _ys (Right q)) = print ("feed1",x)  >> q (Just (L.pack x))
        feeds x (CPartial _ys (Left q)) = print ("feed1 - no request")  >> q
        feeds _x y = return y

feedNothing :: (Monad t) => CompResult y t1 t2 t t3 -> t (CompResult y t1 t2 t t3)
feedNothing (CPartial _ys (Right q)) = q Nothing
feedNothing (CPartial _ys (Left q)) = q
feedNothing x = return x
