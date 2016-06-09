{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TypedChans where

import Control.Monad (forever, void)
import Control.Concurrent
import Control.Concurrent.STM

import Data.Foldable (traverse_)
import Data.Coerce

import Text.Printf

import Logger

data TChanState
  = WriteOnly
  | Readable

newtype TChan' (t :: TChanState) a = TChan' (TChan a)

newBroadcastTChanIO' :: forall a. IO (TChan' 'WriteOnly a)
newBroadcastTChanIO' = coerce (newBroadcastTChanIO :: IO (TChan a))
{-# INLINE newBroadcastTChanIO' #-}

dupTChan' :: TChan' 'WriteOnly a -> STM (TChan' 'Readable a)
dupTChan' (TChan' t) = coerce (dupTChan t)
{-# INLINE dupTChan' #-}

writeTChan' :: TChan' 'WriteOnly a -> a -> STM ()
writeTChan' (TChan' t) = writeTChan t
{-# INLINE writeTChan' #-}

readTChan' :: TChan' 'Readable a -> STM a
readTChan' (TChan' t) = readTChan t
{-# INLINE readTChan' #-}

listener :: Logger -> TChan' 'WriteOnly String -> Int -> IO ()
listener l b i = do
  c <- atomically (dupTChan' b)
  forever $ do
    m <- atomically (readTChan' c)
    logMessage l (printf "%d) %s" i m)

test :: IO ()
test = do
  b <- newBroadcastTChanIO'
  l <- newLogger

  traverse_ (forkIO . listener l b) [1..5]

  void . forkIO . forever $ do
    atomically (writeTChan' b "foo")
    threadDelay (10 ^ (6 :: Int))

  getChar >> stopLogger l
