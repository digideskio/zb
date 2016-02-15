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
          | TString
          | TFunction [Type] Type  -- args, return
          deriving (Show)

data Function = Function String [(String, Type)] Type [Statement] deriving (Show)

data Exp = EInteger Integer
         | ESymbol String
         | EString String
         deriving (Show)

data Statement = SFuncall String [Exp]
               | SReturn Exp
               deriving (Show)

type Env = Map String Type

emptyEnv :: Env
emptyEnv = Map.empty

top :: Env -> SExp -> (Env, Function)
top env sexp =
        case sexp of
             SList (SAtom "func" : SAtom fnname : SList args : ret : rest) -> func env fnname args ret rest
             _ -> error "top"

resolveBinding :: SExp -> (String, Type)
resolveBinding (SList [SAtom name, typ]) = (name, resolveType typ)
resolveBinding _ = error "resolveBinding"

resolveType :: SExp -> Type
resolveType (SAtom "integer") = TInteger
resolveType (SAtom "symbol") = TSymbol
resolveType (SAtom "string") = TString
resolveType _ = error "resolveType"

func :: Env -> String -> [SExp] -> SExp -> [SExp] -> (Env, Function)
func env fnname args ret rest =
        let argBindings = map resolveBinding args
            retType = resolveType ret

            localEnv = foldr (uncurry Map.insert) emptyEnv argBindings

            fn = Function fnname argBindings retType []
            fnt = TFunction (map snd argBindings) retType
        in
        (Map.insert fnname fnt env, fn)
