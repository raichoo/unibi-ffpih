{-# LANGUAGE QuasiQuotes #-}
module HListIndexDemo where

import HList

test :: IO ()
test = do
  let h = "string" ::: True ::: 'c' ::: HNil

  print (hlookup [hindex|0|] h)
  print (hlookup [hindex|1|] h)
  print (hlookup [hindex|2|] h)
