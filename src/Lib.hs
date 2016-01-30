{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parse
    , Sexp
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Control.Applicative ((<*>), (*>), (<$>), (<|>), pure)
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)

data Sexp = SAtom String | SList [Sexp] deriving (Show)

parse :: String -> Sexp
parse s =
  case A.parseOnly sexp (T.pack s) of
       Left e -> error e
       Right r -> r

sexp :: Parser Sexp
sexp = atom <|> list
  where atom = SAtom . T.unpack <$> A.takeWhile1 (A.inClass "a-zA-Z0-9")
        list = SList <$> (A.string "(" *> AC.manyTill (A.skipSpace *> sexp <* A.skipSpace) (A.string ")"))
