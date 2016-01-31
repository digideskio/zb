module Main where

import qualified Data.Map.Strict as Map
import Lib

env :: [(String, Type, Value)]
env = [
        ("+",
         TFunction [] (Just TInteger) TInteger,
         VBuiltin (\args -> VInteger $ sum (map (\v -> case v of VInteger n -> n
                                                                 _ -> error "uh oh") args)))
      ]

main :: IO ()
main = do
        ast <- fmap parse getLine
        print ast
        let le = Map.fromList (map (\(s,t,_) -> (s,t)) env)
        let aexp = compile le ast
        print aexp
