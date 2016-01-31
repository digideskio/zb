{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , compile
    , Sexp
    , Aexp
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
import Text.Read (readMaybe)

data Sexp = SAtom String
          | SList [Sexp]
          deriving (Show)

parse :: String -> Sexp
parse str = case A.parseOnly sexp (T.pack str) of
                 Left e -> error e
                 Right r -> r

sexp :: Parser Sexp
sexp = atom <|> list
       where atom = SAtom . T.unpack <$> A.takeWhile1 (A.inClass "a-zA-Z0-9")
             list = SList <$> (A.string "(" *> AC.manyTill (A.skipSpace *> sexp <* A.skipSpace) (A.string ")"))

data Lit = LInt Integer
         deriving (Show)

data Aexp = ALiteral Lit
          | AVariable String
          | AFuncall Aexp [Aexp]
          deriving (Show)

compile :: Sexp -> Aexp
compile s = case s of
                 SAtom str -> case readMaybe str of
                                   Just n -> ALiteral (LInt n)
                                   Nothing -> AVariable str

                 SList [] -> error "empty cell"
                 SList sexps -> let (h:t) = map compile sexps in
                                AFuncall h t
