module Chans where

import Control.Monad (forever, void)
import Control.Concurrent
import Control.Concurrent.STM

import Data.Foldable (traverse_)

import Text.Printf

import Logger

listener :: Logger -> TChan String -> Int -> IO ()
listener l b i = do
  c <- atomically (dupTChan b)
  forever $ do
    m <- atomically (readTChan c)
    logMessage l (printf "%d) %s" i m)

test :: IO ()
test = do
  b <- newBroadcastTChanIO
  l <- newLogger

  traverse_ (forkIO . listener l b) [1..5]

  void . forkIO . forever $ do
    atomically (writeTChan b "foo")
    threadDelay (10 ^ (6 :: Int))

  getChar >> stopLogger l
