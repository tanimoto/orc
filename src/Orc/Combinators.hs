
--------------------------------------------------------------------------------
-- |
-- Module      : Orc Combinators
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Orc.Combinators where

import Orc.Monad
import qualified Control.Concurrent.StdInOut as S
import Control.DeepSeq


------------------

signal :: Orc ()
signal = return ()

------------------
-- | Cut executes an orc expression, waits for the first result, and then
--   suppresses the rest, including killing any threads involved
--   in computing the remainder. 

cut :: Orc a -> Orc a
cut = join . eagerly

onlyUntil :: Orc a -> Orc b -> Orc b
p `onlyUntil` done = cut (silent p <|> done)

butAfter :: Orc a -> (Float, Orc a) -> Orc a
p `butAfter` (t,def) = cut (p <|> (delay t >> def))

timeout :: Float -> a -> Orc a -> Orc a
timeout n a p = cut (p <|> (delay n >> return a))

silent :: Orc a -> Orc b
silent p = p >> stop

liftList :: (MonadPlus list) => [a] -> list a
liftList ps = foldr mplus mzero $ map return ps

-- repeating works best when p is single-valued
repeating :: Orc a -> Orc a
repeating p = do
    x <- p
    return x <|> repeating p

runChan :: Chan a -> Orc a -> IO ()
runChan ch p = runOrc $ (p >>= writeChan ch)

--------------------

sync :: (a->b->c) -> Orc a -> Orc b -> Orc c
sync f p q = do
  po <- eagerly p
  qo <- eagerly q
  pure f <*> po <*> qo

notBefore:: Orc a -> Float -> Orc a
p `notBefore` w = sync const p (delay w)

syncList :: [Orc a] -> Orc [a]
syncList ps = sequence (map eagerly ps) >>= sequence


---------------------------------------------------------------------------
-- | Wait for a period of w seconds, then continue processing.

delay :: (RealFrac a) => a -> Orc ()
delay w =  (liftIO $ threadDelay (round (w * 1000000)))
       <|> (silent $ do
             guard (w>100)
             putStrLine ("Just checking you meant to wait "
                           ++show w++" seconds"))
    
---------------------------------------------------------------------------
-- | 'printOrc' and 'prompt' uses the 'Stdinout' library to provide
-- basic console input/output in a concurrent setting. 'runOrc' executes
-- an orc expression and prints out the answers eagerly per line.

printOrc :: Show a => Orc a -> IO ()
printOrc p = S.setupStdInOut $ runOrc $ do
    a <- p
    putStrLine ("Ans = " ++ show a)

prompt :: String -> Orc String
prompt str = liftIO $ S.prompt str

putStrLine :: String -> Orc ()
putStrLine str = liftIO $ S.putStrLine str


---------------------------------------------------------------------------

scan :: (a -> s -> s) -> s -> Orc a -> Orc s
scan f s p = do
  accum <- newTVar s
  x <- p
  (w,w') <- modifyTVar accum (f x)
  return w'

(<?>) :: Orc a -> Orc a -> Orc a
p <?> q = do
    tripwire <- newEmptyMVar
    do x <- p
       tryPutMVar tripwire ()
       return x
     <+>
     do triggered <- tryTakeMVar tripwire
        case triggered of
          Nothing -> q
          Just _  -> stop

count :: Orc a -> Orc (Either a Int)
count p = do
    accum <- newTVar 0
    do x <- p
       modifyTVar accum (+1)
       return $ Left x
     <+>
       fmap Right (readTVar accum)

collect :: Orc a -> Orc [a]
collect p = do
    accum <- newTVar []
    silent (do x <- p
               modifyTVar accum (x:))
     <+>
       readTVar accum


---------------------------------------------------------------------------
-- | List-like functions

takeOrc :: Int -> Orc a -> Orc a
takeOrc n p = do
    vals <- newEmptyMVar
    end  <- newEmptyMVar
    echo n vals end <|> silent (sandbox p vals end)

dropOrc :: Int -> Orc a -> Orc a
dropOrc n p = do
    countdown <- newTVar n
    x <- p
    join $ atomically $ do 
      w <- readTVarSTM countdown
      if w==0 then return $ return x
        else do
          writeTVarSTM countdown (w-1)
          return stop

zipOrc :: Orc a -> Orc b -> Orc (a,b)
zipOrc p q = do
    pvals <- newEmptyMVar
    qvals <- newEmptyMVar
    end   <- newEmptyMVar
    zipp pvals qvals end
      <|> silent (sandbox p pvals end)
      <|> silent (sandbox q qvals end)

--------------
-- Auxilliary definitions

sandbox :: Orc a -> MVar (Maybe a) -> MVar () -> Orc ()
sandbox p vals end
  = ((p >>= (putMVar vals . Just)) <+> putMVar vals Nothing)
    `onlyUntil` takeMVar end 

echo :: Int -> MVar (Maybe a) -> MVar () -> Orc a
echo 0  _   end = silent (putMVar end ())
echo j vals end = do
    mx <- takeMVar vals
    case mx of
      Nothing -> silent (putMVar end ())
      Just x  -> return x <|> echo (j-1) vals end

zipp :: MVar (Maybe a) -> MVar (Maybe b) -> MVar () -> Orc (a,b)
zipp pvals qvals end = do
    mx <- takeMVar pvals
    my <- takeMVar qvals
    case mx of
      Nothing -> silent (putMVar end () >> putMVar end ())
      Just x  -> case my of
        Nothing -> silent (putMVar end () >> putMVar end ())
        Just y  -> return (x,y) <|> zipp pvals qvals end



---------------------------------------------------------------------------
-- | Publish is a hyperstrict form of return. It is useful
--   for combining results from multiple 'val' computations, providing
--   a synchronization point. 

publish :: NFData a => a -> Orc a
publish x = deepseq x $ return x

