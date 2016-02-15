{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    , SExpr(..)
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)


data SExpr = SAtom String
           | SInteger Integer
           | SSymbol String
           | SString String
           | SList [SExpr]
           deriving (Show)

parse :: String -> [SExpr]
parse str = case A.parseOnly (AC.many1 sexpr) (T.pack str) of
                 Left e -> error ("parse: " ++ e)
                 Right r -> r

sexpr :: Parser SExpr
sexpr = integer <|> atom <|> symbol <|> list <|> string
        where integer = SInteger . read . T.unpack <$> A.takeWhile1 (A.inClass "0-9")
              -- broken: "1a" will read as (SInteger 1) (SAtom "a").
              -- should maybe error or something instead.
              -- should read until " \t\r\n)" or EOF. anything else is a mistake
              atom = SAtom . T.unpack <$> atom_symbol_inner
              symbol = SSymbol . T.unpack <$> (A.string ":" *> atom_symbol_inner)
              atom_symbol_inner = A.takeWhile1 (A.inClass "-a-zA-Z0-9+*/_=")
              list = SList <$> (A.string "(" *> AC.manyTill (A.skipSpace *> sexpr <* A.skipSpace) (A.string ")"))
              string = SString . T.unpack <$> (A.string "\"" *> A.takeTill (== '"') <* A.string "\"")
