module Compiler
    ( compile
    , Type(..)
    , TAST
    , Value(..)
    ) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Parser (AST(..))

{-
 - Start with a statement based language (!!) and work our way up.
 -}

data Type = TInteger
          | TFunction [Type] (Maybe Type) Type  -- args, *args, return
          deriving (Show)

data Literal = LInteger Integer
             deriving (Show)

data TAST = ALiteral Literal
          | AVariable String Type
          | AFuncall TAST [TAST] Type  -- head tail type
          deriving (Show)

data Value = VInteger Integer
           | VBuiltin ([Value] -> Value)

type LexicalEnv = Map String Type

compile :: LexicalEnv -> AST -> TAST
compile le s = case s of
                    SAtom str -> case Map.lookup str le of
                                      Just typ -> AVariable str typ
                                      Nothing -> error ("unbound variable " ++ str)
                    SInteger n -> ALiteral (LInteger n)
                    SList sexps -> case map (compile le) sexps of
                                        [] -> error "empty cell"
                                        h:t -> AFuncall h t (outtype h)

gettype :: TAST -> Type
gettype (ALiteral (LInteger _)) = TInteger
gettype (AVariable _ typ) = typ
gettype (AFuncall _ _ typ) = typ

outtype :: TAST -> Type
outtype t = case gettype t of
                 TFunction _ _ typ -> typ
                 _ -> error "can't get outtype of non-function"
