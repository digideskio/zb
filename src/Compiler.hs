module Compiler
    ( Type(..)
    , top
    , Function
    , Env
    , emptyEnv
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Parser (SExp(..))

{-
 - Start with a statement based language (!!) and work our way up.
 -
 - I don't plan to stay in s-exps forever, so no QUOTE or anything below.
 -
 - Let's start with this:
 -
 - ;; (= (type-of main) (list (list) :integer))
 - (func main ()
 -   (print "Hi")
 -   (return 0))
 -}

data Type = TInteger
          | TSymbol
          | TFunction [Type] Type  -- args, return
          deriving (Show)

data Function = Function String [(String, Type)] [Statement] deriving (Show)

data Statement = Statement deriving (Show)

type Env = Map String Type

emptyEnv :: Env
emptyEnv = Map.empty

top :: Env -> SExp -> (Env, Function)
top env sexp =
        case sexp of
             SList (SAtom "func" : SAtom fnname : SList args : rest) -> func env fnname args rest
             _ -> error "bad form at top level"

func :: Env -> String -> [SExp] -> [SExp] -> (Env, Function)
func env fnname args rest =
        -- TODO: parse statements, type everything, add TFunction to env
        -- and return fn.
        let fn = Function fnname [] [] in
        (env, fn)
