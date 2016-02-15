module Main where

import Parser (parse, SExp)
import Compiler (Env, Function, top, emptyEnv)

main :: IO ()
main = do
        let sexps = parse $ unlines [
                    "(func main ((arg string)) integer",
                    "  (print \"Hi!\")",
                    "  (return 0))"
                    ]
        print sexps
        let (env, fns) = foldr applySexp (emptyEnv, []) sexps
        print env
        print fns

        where applySexp :: SExp -> (Env, [Function]) -> (Env, [Function])
              applySexp sexp (env, fns) =
                      let (env', fn') = top env sexp in
                      (env', fn':fns)
