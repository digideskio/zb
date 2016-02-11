module Main where

import Parser (parse, SExp)
import Compiler (Env, Function, top, emptyEnv)

main :: IO ()
main = do
        sexps <- fmap parse getLine
        print sexps
        let (env, fns) = foldr applySexp (emptyEnv, []) sexps
        print env
        print fns

        where applySexp :: SExp -> (Env, [Function]) -> (Env, [Function])
              applySexp sexp (env, fns) =
                      let (env', fn') = top env sexp in
                      (env', fn':fns)
