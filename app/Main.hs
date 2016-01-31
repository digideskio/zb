module Main where

import Lib

main :: IO ()
main = do
        sexp <- fmap parse getLine
        print sexp
        let aexp = compile sexp
        print aexp
