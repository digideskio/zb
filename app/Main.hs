module Main where

import Lib

main :: IO ()
main =
  fmap parse getLine >>= print
