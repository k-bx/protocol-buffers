module Text.ProtocolBuffers.MyGet(CERWS(CERWS,unCERWS),runCERWS,getCC,getCC1) where

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

strictToLazy :: S.ByteString -> L.ByteString
strictToLazy x
  | S.null x = L.Empty
  | otherwise = L.Chunk x L.Empty

lazyToStrict :: L.ByteString -> S.ByteString
lazyToStrict = S.concat . L.toChunks

part1 = L.pack [1..9]
part2 = L.pack [10]
part3 = L.pack [11..19]

thing1 = getBytes 10 >> throwError "WTF"
thing2 = getBytes 9

testG = runGet (catchError thing1 (\e -> liftIO (putStrLn $ "caught " ++ show e) >> getBytes 10 ))

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

type Get b a = CERWS (IResult b) String () () S IO a

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
     else suspend $ getBytes n
--   else return $ IPartial (undefined {- \s' -> unGet (getBytes n) (S (L.append s $ strictToLazy s') offset (s' : adds) ) cont-})

runGet :: Get a a -> L.ByteString -> IO (IResult a)
runGet g bs = let s = S bs 0 mempty
              in unGet g s (\a _ _ s' -> return (IFinished s' a))
  where unGet (CERWS f) = \s sc -> f sc ec PartialEmpty () () s
            where ec e = return (IFailed e)
{-
        pc (S bs n future) go = let f bs' = let s = S (mappend bs bs') n (future |> bs')
                                            in go ec s
                                in return (IPartial f)
-}

appendBS (S bs n future) bs' = S (mappend bs bs') n (future |> bs')

suspend :: Get b a -> Get b a
suspend todo = CERWS $ (\ sc _ec pcIn r w s0 ->
  let go ecOut pcOut PartialEmpty =
          let f bs' = let s0' = appendBS s0 bs'
                      in unCERWS todo sc ecOut pcOut () () s0'
          in return (IPartial f)
{-
      go ec (PartialFrame s1 make_ec with_ec) =
          let f bs' = let s1' = appendBS s1 s1'
                          ec' = make_ec s1'
                          pc' = with_ec ec'
                      in unCERWS sc ec' pc' r w s0'
          in return undefined
-}
  in go (return . IFailed) PartialEmpty pcIn)

data PartialContinuation b = PartialEmpty 
                           | PartialFrame S (S -> String -> IO b)
                                            ((String -> IO b) -> PartialContinuation b)

newtype CERWS b e r w s m a = CERWS {
      unCERWS :: (a -> r -> w -> s -> m b) -- success continuation
              -> (e -> m b)                -- error continuation
              -> PartialContinuation b     -- partial continuation (simple for now)
              -> r                         -- reader
              -> w                         -- log so far
              -> s                         -- state
              -> m b                       -- operation
    }

runCERWS :: (Monad m,Monoid w) =>
            r -> s -> CERWS (Either e (a,w,s)) e r w s m a
         -> m (Either e (a,w,s))
runCERWS r s1 m = unCERWS m sc ec undefined r mempty s1
  where sc a _ w s2 = return (Right (a,w,s2))
        ec e = return (Left e)

instance (Monad m,Error e) => Functor (CERWS b e r w s m) where
--fmap f m = m >>= return . f
  fmap f m = CERWS (\sc -> unCERWS m (sc . f))

instance (Monad m,Error e) => Monad (CERWS b e r w s m) where
  return a = CERWS (\sc _ _ -> sc a)
  m >>= k  = CERWS (\sc ec pc -> unCERWS m (\a -> unCERWS (k a) sc ec pc) ec pc)
  fail msg = CERWS (\_ ec _ _ _ _ -> ec (strMsg msg))

instance MonadTrans (CERWS b e r w s) where
  lift m = CERWS (\sc ec pc r w s -> m >>= \a -> sc a r w s)

instance (MonadIO m,Error e) => MonadIO (CERWS b e r w s m) where
  liftIO = lift . liftIO

instance (Monad m, Error e) => MonadCont (CERWS b e r w s m) where
  callCC = callCC_CERWS

callCC_CERWS :: ((a -> CERWS b e1 r w s m a1) -> CERWS b e r w s m a)
             -> CERWS b e r w s m a
callCC_CERWS f = CERWS $ \sc -> let k a = CERWS (\_ _ _ -> sc a)
                                in unCERWS (f k) sc

instance (Monad m,Error e) => MonadError e (CERWS b e r w s m) where
  throwError msg = CERWS (\_ ec _ _ _ _ -> ec msg)
  catchError = catchError_CERWS

catchError_CERWS :: CERWS b e {-e1-} r w s m a
                 -> (e {-e1-} -> CERWS b e r w s m a)
                 -> CERWS b e r w s m a
catchError_CERWS m handler = CERWS $ \sc ec pc r w s ->
  let ec' e' = unCERWS (handler e') sc ec pc r w s
{-
      pc' sIn goIn = let goIn' goEC goS = undefined
                     in pc goIn'
-}
  in unCERWS m sc ec' pc r w s

instance (Monad m, Error e, Monoid w) => MonadWriter w (CERWS b e r w s m) where
  tell w'  = CERWS (\sc _ _ r w -> sc () r (mappend w w'))
  listen m = CERWS (\sc -> unCERWS m (\ a    r' w'-> sc (a,w') r'    w' ))
  pass m   = CERWS (\sc -> unCERWS m (\(a,f) r' w'-> sc  a     r' (f w')))

instance (Monad m, Error e) => MonadReader r (CERWS b e r w s m) where
  ask = CERWS (\sc _ _ r -> sc r r)
  local f m = CERWS (\sc ec pc r -> let sc' a _ = sc' a r
                                    in unCERWS m sc' ec pc (f r))
              
instance (Monad m,Error e) => MonadState s (CERWS b e r w s m) where
  get   = CERWS (\sc _ _ r w s -> sc s  r w s)
  put s = CERWS (\sc _ _ r w _ -> sc () r w s)

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
 where print :: (MonadIO m, Show a) => a -> m ()
       print = liftIO . Prelude.print

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
 where print :: (MonadIO m, Show a) => a -> m ()
       print = liftIO . Prelude.print

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
 where print = liftIO . Prelude.print

