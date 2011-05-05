
--------------------------------------------------------------------------------
-- |
-- Module      : Orc Monad
-- Copyright   : (c) 2008-2010 Galois, Inc.
-- License     : BSD3
--
-- Maintainer  : John Launchbury <john@galois.com>
-- Stability   :
-- Portability : concurrency
--
-- The Orc EDSL in Haskell

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}


module Orc.Monad (
    Orc              -- :: * -> *   
  , module Control.Monad
  , module Control.Applicative
  , module Control.Concurrent.MonadIO
  , module Control.Concurrent.STM.MonadIO
  , stop             -- :: Orc a
  , eagerly          -- :: Orc a -> Orc (Orc a)
  , val              -- :: Orc a -> Orc a
  , (<+>)            -- :: Orc a -> Orc a -> Orc a
  , runOrc           -- :: Orc a -> IO ()

  ) where


import Control.Monad
import Control.Applicative
import Control.Concurrent.MonadIO
import Control.Concurrent.STM.MonadIO
import Control.Concurrent.Hierarchical

import System.IO.Unsafe


---------------------------------------------------------------------------

newtype Orc a = Orc {(#) :: (a -> HIO ()) -> HIO ()}

instance Functor Orc where
  fmap f p = Orc $ \k -> p # (k . f)

instance Monad Orc where
  return x = Orc $ \k -> k x
  p >>= h  = Orc $ \k -> p # (\x -> h x # k)
  fail _   = stop

stop :: Orc a
stop = Orc $ \_ -> return ()

instance Alternative Orc where
  empty = stop
  (<|>) = par

par :: Orc a -> Orc a -> Orc a
par p q = Orc $ \k -> do
    fork (p # k)
    q # k
{- Fully symmetric version: relevant if using async exceptions etc.
    fork (q # k)
    return ()
-}


instance MonadIO Orc where
  liftIO io = Orc (liftIO io >>=)

runOrc :: Orc a -> IO ()
runOrc p = runHIO (p # \_ -> return ())

instance Applicative Orc where
  pure  = return
  f <*> x = ap f x

instance MonadPlus Orc where
  mzero = empty
  mplus = (<|>)


---------------------------------------------------------------------------

(<+>) :: Orc a -> Orc a -> Orc a
p <+> q = Orc $ \k -> do
    w <- newGroup
    local w $ fork (p # k)
    finished w
    q # k

eagerly :: Orc a -> Orc (Orc a)
eagerly p = Orc $ \k -> do
    res <- newEmptyMVar
    w <- newGroup
    local w $ fork (p `saveOnce` (res,w))
    k (liftIO $ readMVar res)

val :: Orc a -> Orc a
val p = Orc $ \k -> do
    res <- newEmptyMVar
    w <- newGroup
    local w $ fork (p `saveOnce` (res,w))
    k (unsafePerformIO $ readMVar res)      -- Like unsafeInterleaveIO

saveOnce :: Orc a -> (MVar a,Group) -> HIO ()
p `saveOnce` (r,w) = do 
    ticket <- newMVar ()
    p # \x -> (takeMVar ticket >> putMVar r x >> close w)


---------------------------------------------------------------------------
