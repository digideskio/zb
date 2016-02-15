module Main where

import qualified Data.Map.Strict as Map
import Parser (parse, SExpr)
import Compiler (Env, Function, Type(..), top, emptyEnv)

main :: IO ()
main = do
        let sexps = parse $ unlines [
                    "(func main ((arg string)) integer",
                    "  (print \"Hi!\")",
                    "  (print arg)",
                    "  (return (print \"oh\")))"
                    ]

        let env = Map.insert "print" (TFunction [TString] TInteger) emptyEnv
        let (_, fns) = foldr applySexp (env, []) sexps
        putStrLn "fns: "
        print fns

        where applySexp :: SExpr -> (Env, [Function]) -> (Env, [Function])
              applySexp sexpr (env, fns) =
                      let (env', fn') = top env sexpr in
                      (env', fn':fns)
