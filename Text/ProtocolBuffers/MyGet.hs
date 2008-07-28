module Text.ProtocolBuffers.MyGet(CERWS(CERWS,unCERWS),runCERWS,getCC,getCC1) where

import qualified Prelude as P(print)
import Prelude hiding (print)

import Data.Monoid(Monoid(mempty,mappend))
import Control.Monad.Fix(fix)
import Control.Monad(MonadPlus(mzero,mplus))
import Control.Monad.Trans(MonadTrans(lift),MonadIO(liftIO))
import Control.Monad.Cont.Class(MonadCont(callCC))
import Control.Monad.Error.Class(MonadError(throwError,catchError),Error(strMsg,noMsg))
import Control.Monad.Reader.Class(MonadReader(ask,local))
import Control.Monad.Writer.Class(MonadWriter(tell,listen,pass))
import Control.Monad.State.Class(MonadState(get,put))

import Data.Int(Int64)
import Data.Sequence(Seq,viewl,ViewL(..),(|>))
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L(ByteString(Empty,Chunk),chunk)

import Text.Show.Functions

print :: (MonadIO m, Show a) => a -> m ()
print s = liftIO (P.print s)


strictToLazy :: S.ByteString -> L.ByteString
strictToLazy x
  | S.null x = L.Empty
  | otherwise = L.Chunk x L.Empty

lazyToStrict :: L.ByteString -> S.ByteString
lazyToStrict = S.concat . L.toChunks

part1 = L.pack [48..56]
part2 = L.pack [57]
part3 = L.pack [58..66]

diag :: String -> Get b ()
diag msg = do print ("diag: "++msg)
              get >>= print
              getPC >>= print


thing1 = diag "enter thing1" >> getBytes 10 >> diag "after getBytes 10 in thing1"  >> throwError "WTF" >> print "PAST throwError!!!!"
thing2 = diag "enter thing2" >> getBytes 9 >> diag "after getBytes 9 in thing2"

testG = runGet (catchError (thing1) (\e -> liftIO (putStrLn $ "caught " ++ show e) >> diag "Enter error handler" >> getBytes 10 >> diag "After getbytes 10 in error handler" ))

feed :: forall t. L.ByteString -> IResult t -> IO (IResult t)
feed bs (IPartial f) = f bs

data IResult a = IFailed String
               | IFinished S a
               | IPartial (L.ByteString -> IO (IResult a))
  deriving Show

data S = S { current :: L.ByteString
           , consumed :: Int64
           , future :: Seq L.ByteString
           }
  deriving Show

type Get b = CERWS (IResult b) String () () S IO

-- | Pull @n@ bytes from the input, as a strict ByteString.
getBytes :: Int -> Get r S.ByteString
getBytes n = do
  (S bs offset future) <- get
  if fromIntegral n <= L.length bs
     then let (consume, rest) = L.splitAt (fromIntegral n) bs
           in do let s = S rest (offset + fromIntegral n) future
                     a = lazyToStrict consume
                 put s
                 return a
     else suspend >> getBytes n
--   else return $ IPartial (undefined {- \s' -> unGet (getBytes n) (S (L.append s $ strictToLazy s') offset (s' : adds) ) cont-})

runGet :: Get a a -> L.ByteString -> IO (IResult a)
runGet g bs = unGet g sIn scIn
  where sIn = S bs 0 mempty
        scIn = (\a _ _ sOut _ -> return (IFinished sOut a)) 
        unGet (CERWS f) = \s sc -> f sc () () s (PartialEmpty ec)
            where ec e = return (IFailed e)
{-
        pc (S bs n future) go = let f bs' = let s = S (mappend bs bs') n (future |> bs')
                                            in go ec s
                                in return (IPartial f)
-}

class MonadSuspend m where
  suspend :: m ()

appendBS (S bs n future) bs' = S (mappend bs bs') n (future |> bs')
addFuture bs (PartialFrame catcher s pc) = PartialFrame catcher 1(appendBS s bs) (addFuture bs pc)
addFuture bs x@(PartialEmpty _) = x

instance MonadSuspend (Get b) where
    suspend = CERWS $ (\ sc r w sIn pcIn ->
      let f bs = let sOut = appendBS sIn bs
                     pcOut = addFuture bs pcIn
                 in sc () r w sOut pcOut
      in return (IPartial f))
    
getPC :: CERWS b e r w s m (PartialContinuation b e s m)
getPC = CERWS $ \sc r w s pc -> sc pc r w s pc

type PC b = PartialContinuation b String S IO

data PartialContinuation b e s m = PartialEmpty (e -> m b)
                                 | PartialFrame (s -> PartialContinuation b e s m -> e -> m b)
                                                s
                                                (PartialContinuation b e s m)
  deriving Show

type SuccessContinuation b e r w s m a = (a -> r -> w -> s -> PartialContinuation b e s m -> m b)

newtype CERWS b e r w s m a = CERWS {
      unCERWS :: SuccessContinuation b e r w s m a
              -> r                         -- reader
              -> w                         -- log so far
              -> s                         -- state
              -> PartialContinuation b e s m -- error stack state
              -> m b                       -- operation
    }

runCERWS :: (Monad m,Monoid w)
         => r -> s -> CERWS (Either e (a,w,s)) e r w s m a
         -> m (Either e (a,w,s))
runCERWS r sIn m = unCERWS m sc r mempty sIn pc
  where sc a _ wOut sOut _ = return (Right (a,wOut,sOut))
        pc = PartialEmpty (\e -> return (Left e))

instance (Monad m,Error e) => Functor (CERWS b e r w s m) where
--fmap f m = m >>= return . f
  fmap f m = CERWS (\sc -> unCERWS m (sc . f))

instance (Monad m,Error e) => Monad (CERWS b e r w s m) where
  return a = CERWS (\sc -> sc a)
  m >>= k  = CERWS (\sc -> unCERWS m (\a -> unCERWS (k a) sc))
  fail msg = throwError (strMsg msg)

instance MonadTrans (CERWS b e r w s) where
  lift m = CERWS (\sc r w s pc -> m >>= \a -> sc a r w s pc)

instance (MonadIO m,Error e) => MonadIO (CERWS b e r w s m) where
  liftIO = lift . liftIO

instance (Monad m, Error e) => MonadCont (CERWS b e r w s m) where
  callCC = callCC_CERWS

--callCC_CERWS :: ((a -> CERWS b e1 r w s m a1) -> CERWS b e r w s m a)
--             -> CERWS b e r w s m a
callCC_CERWS f = CERWS $ \sc -> let k a = CERWS (\_ -> sc a)
                                in unCERWS (f k) sc

instance (Monad m,Error e) => MonadError e (CERWS b e r w s m) where
  throwError = throwError_CERWS
  catchError = catchError_CERWS

-- throwError_CERWS :: e -> CERWS b e r w s m a
throwError_CERWS msg = CERWS $ \sc r w s pc ->
  case pc of
    PartialEmpty ec -> ec msg
    PartialFrame catcher s1 pc1 -> catcher s1 pc1 msg

catchError_CERWS :: CERWS b e {-e1-} r w s m a
                 -> (e {-e1-} -> CERWS b e r w s m a)
                 -> CERWS b e r w s m a
catchError_CERWS mayFail handler = CERWS $ \sc r w s pc ->
  let pcWithHandler = let catcher s1 pc1 e1 = unCERWS (handler e1) sc r w s1 pc1
                      in PartialFrame catcher s pc
  in unCERWS mayFail sc r w s pcWithHandler

instance (Monad m, Error e, Monoid w) => MonadWriter w (CERWS b e r w s m) where
  tell w'  = CERWS (\sc r w -> sc () r (mappend w w'))
  listen m = CERWS (\sc -> unCERWS m (\ a    r' w'-> sc (a,w') r'    w' ))
  pass m   = CERWS (\sc -> unCERWS m (\(a,f) r' w'-> sc  a     r' (f w')))

instance (Monad m, Error e) => MonadReader r (CERWS b e r w s m) where
  ask = CERWS (\sc r -> sc r r)
  local f m = CERWS (\sc r -> let sc' a _ = sc' a r
                              in unCERWS m sc' (f r))
              
instance (Monad m,Error e) => MonadState s (CERWS b e r w s m) where
  get   = CERWS (\sc r w s -> sc s  r w s)
  put s = CERWS (\sc r w _ -> sc () r w s)

instance (Monad m, Error e) => MonadPlus (CERWS b e r w s m) where
  mzero = throwError noMsg
  mplus m1 m2 = catchError m1 (const m2)

getCC :: (MonadCont m) => m (m a)
getCC = callCC (return . fix)

getCC1 :: (MonadCont m) => a -> m ( a -> m b ,  a  )
getCC1 x = callCC $ \k -> let jump a = k (jump,a) in return (jump,x)

testGet :: IO (Either String ((),(),Int))
testGet = runCERWS () (10::Int) $ do
  again <- getCC
  s <- get
  if s <= 0 then throwError "Boom"
            else print ("Countdown from "++show s)
  (jump,val) <- getCC1 s
  if val > 0 then print val >> jump (pred val) 
             else get >>= put . (`div` 2) >> again
  return ()

baz :: IO (Either String (String,String,String))
baz = runCERWS () "start" $ do
  print "alpha"
  tell "1"
  print =<< callCC (\k -> return (15::Int))
  put . (++"!") =<< get
  r <- callCC $ \k -> do
         tell "2"
         print "beta"
         put .  (++"@") =<< get
         k 17
         print "DOES NOT PRINT"
         return 1
  print ("r  is "++show (r))
  return "EOF"

test :: IO Bool
test = do x <- runCERWS () "Hello" bar
          print x
          return (x==ans)
  where ans = Right ("Hello was gotten","ABc","write value from listen: a")

type T m = CERWS (Either String (String,String,String)) String () String String m String

bar :: T IO
-- bar :: CERWS (Either String (String,String,String)) String () String String IO String
bar = do
  print "alpha"
  (a,w) <- listen (tell "a" >> print "beta" >> get)
  put ("write value from listen: "++w)
  pass $ catchError (tell "cheese" >> print "delta" >> put "wisconsin" >> throwError "b")
                    (\msg -> tell msg >> print "epsilon" >> return ( (), map (toEnum . (subtract 32) . fromEnum)))
  tell "c"
  print "gamma"
  return (a++" was gotten")

