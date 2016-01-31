{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , compile
    , AST
    , Type(..)
    , TAST
    , Value(..)
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)
import Data.Attoparsec.Text (Parser)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Text.Read (readMaybe)

--
-- parsing
--

data AST = SAtom String
         | SInteger Integer
         | SList [AST]
         deriving (Show)

parse :: String -> AST
parse str = case A.parseOnly ast (T.pack str) of
                 Left e -> error e
                 Right r -> r

ast :: Parser AST
ast = integer <|> atom <|> list
      where integer = SInteger . read . T.unpack <$> A.takeWhile1 (A.inClass "0-9")
            -- broken: "1a" will read as (SInteger 1) (SAtom "a").
            -- should maybe error or something instead.
            -- should read until " \t\r\n)" or EOF. anything else is a mistake
            atom = SAtom . T.unpack <$> A.takeWhile1 (A.inClass "-a-zA-Z0-9+*/_")
            list = SList <$> (A.string "(" *> AC.manyTill (A.skipSpace *> ast <* A.skipSpace) (A.string ")"))

--
-- compiling
--

data Type = TInteger
          | TFunction [Type] (Maybe Type) Type  -- args, *args, return
          deriving (Show)

data Literal = LInteger Integer
             deriving (Show)

data TAST = ALiteral Literal
          | AVariable String Type
          | AFuncall TAST [TAST]
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
                                        h:t -> AFuncall h t
