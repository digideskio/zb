module Main where

import qualified Data.Map.Strict as Map
import Parser (parse, SExpr)
import Compiler (Env, Function(..), Type(..), top, emptyEnv)
import Data.Map.Strict (Map)

main :: IO ()
main = do
        let sexps = parse $ unlines [
                    "(func main ((arg string)) integer",
                    "  (print \"Hi!\")",
                    "  (print arg)",
                    "  (return (print \"oh\")))"
                    ]

        let env = Map.insert "print" (TFunction [TString] TInteger) emptyEnv
        let (_, fns) = foldr applySexp (env, Map.empty) sexps

        print fns

        where applySexp :: SExpr -> (Env, Map String Function) -> (Env, Map String Function)
              applySexp sexpr (env, fns) =
                      let (env', fn'@(Function fnName _ _ _)) = top env sexpr in
                      (env', Map.insert fnName fn' fns)
