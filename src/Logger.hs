{-# LANGUAGE LambdaCase #-}
module Logger where

import Control.Monad (void)
import Control.Concurrent

data Logger = Logger (MVar LogMessage)

data LogMessage
  = Log String
  | Stop (MVar ())

newLogger :: IO Logger
newLogger = do
  q <- newEmptyMVar
  void (forkIO (loop q))
  return (Logger q)
  where
    loop q = takeMVar q >>= \case
      Stop s  -> putMVar s () >> return ()
      Log msg -> putStrLn msg >> loop q

logMessage :: Logger -> String -> IO ()
logMessage (Logger q) msg = putMVar q (Log msg)

stopLogger :: Logger -> IO ()
stopLogger (Logger q) = do
  s <- newEmptyMVar
  putMVar q (Stop s)
  void (takeMVar s)
