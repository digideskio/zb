module Parser
    ( parse
    , AST(..)
    ) where

import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)


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
            list = SList <$> (A.string (T.pack "(") *> AC.manyTill (A.skipSpace *> ast <* A.skipSpace) (A.string (T.pack ")")))
