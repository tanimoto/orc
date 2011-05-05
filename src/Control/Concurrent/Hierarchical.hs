--------------------------------------------------------------------------------
-- |
-- Module      : Hierarchical IO Threads
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency, unsafeIntereaveIO
--
-- Hierarchical concurrent threads

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


module Control.Concurrent.Hierarchical (

    HIO             -- :: * -> *

  , runHIO          -- :: HIO b -> IO b
  , newPrimGroup
  , newGroup     -- :: HIO Group
  
  , local
  , close
  , Group
  , finished
  
  ) where

import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO
import System.IO.Unsafe          -- Global variable for profiling code


---------------------------------------------------------------------------
-- | 

newtype HIO a = HIO {inGroup :: Group -> IO a}

instance Functor HIO where
  fmap f (HIO hio) = HIO (fmap (fmap f) hio)

instance Monad HIO where
  return x = HIO $ \_ -> return x
  m >>= k  = HIO $ \w -> do
                 x <- m `inGroup` w
                 k x `inGroup` w


instance Applicative HIO where
  pure  = return
  f <*> x = ap f x

instance MonadIO HIO where
  liftIO io = HIO $ const io


---------------------------------------------------------------------------
-- | The thread-registry environment is a hierarchical structure of local
-- thread neighborhoods. 

type Group       = (TVar Int, TVar Inhabitants)
data Inhabitants = Closed | Open [Entry]
data Entry       = Thread ThreadId
                 | Group Group


instance HasFork HIO where
  fork hio = HIO $ \w -> block $ do
    when countingThreads incrementThreadCount
    increment w
    fork (block (do tid <- myThreadId
                    register (Thread tid) w
                    unblock (hio `inGroup` w))
          `finally` 
            decrement w)


newGroup :: HIO Group
newGroup = HIO $ \w -> do
    w' <- newPrimGroup
    register (Group w') w
    return w'

local :: Group -> HIO a -> HIO a 
local w p = liftIO (p `inGroup` w)

close :: Group -> HIO ()
close (c,t) = liftIO $ fork (kill (Group (c,t)) >> writeTVar c 0)
              >> return ()

finished :: Group -> HIO ()
finished w = liftIO $ isZero w


runHIO :: HIO b -> IO b
runHIO hio = do
    w <- newPrimGroup
    r <- hio `inGroup` w
    isZero w
    when countingThreads printThreadReport
    return r

newPrimGroup :: IO Group
newPrimGroup = do
  count   <- newTVar 0
  threads <- newTVar (Open [])
  return (count,threads)

register :: Entry -> Group -> IO ()
register tid (_,t) = join $ atomically $ do
  ts <- readTVarSTM t
  case ts of
    Closed    -> return (myThreadId >>= killThread)     -- suicide
    Open tids -> writeTVarSTM t (Open (tid:tids)) >>    -- register
                 return (return ())


kill :: Entry -> IO ()
kill (Thread tid)  = killThread tid
kill (Group (_,t)) = do
  (ts,_) <- modifyTVar t (const Closed)
  case ts of
    Closed    -> return ()
    Open tids -> sequence_ (map kill tids)

increment, decrement, isZero :: Group -> IO ()
increment (c,_) = modifyTVar_ c (+1)
decrement (c,_) = modifyTVar_ c (\x->x-1)
isZero    (c,_) = atomically $ (readTVarSTM c >>= (check . (==0)))
                      -- block until set (i.e. when locality is finished)


---------------------------------------------------------------------------
--  Profiling code: Records how many threads were created

countingThreads :: Bool 
countingThreads = True          -- set to False to disable reporting

threadCount :: TVar Integer                            
threadCount = unsafePerformIO $ newTVar 0            

incrementThreadCount :: IO ()
incrementThreadCount = modifyTVar_ threadCount (+1)       

printThreadReport :: IO ()
printThreadReport = do
    n <- readTVar threadCount          
    putStrLn "----------------------------"      
    putStrLn (show n ++ " HIO threads were forked")   

