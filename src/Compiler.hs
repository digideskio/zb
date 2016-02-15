module Compiler
    ( Type(..)
    , top
    , Function
    , Env
    , emptyEnv
    ) where

import qualified Data.Map.Strict as Map
import Data.Foldable (foldl')
import Data.Map.Strict (Map)

import Parser (SExpr(..))

{-
 - Start with a statement based language (!!) and work our way up.
 -
 - I don't plan to stay in s-exps forever, so no QUOTE or anything below.
 -
 - Let's start with this:
 -
 - ;; (= (type-of main) (list (list :string) :integer))
 - ;;               --> [[:string] :integer]
 - (func main ((arg string)) integer
 -   (print "Hi")
 -   (return 0))
 -}

data Type = TInteger
          | TSymbol
          | TString
          | TFunction [Type] Type  -- args, return
          deriving (Show, Eq)

data Function = Function String [(String, Type)] Type [Statement] deriving (Show)

data Expr = EInteger Integer
          | ESymbol String
          | EString String
          | ELookup String Type
          deriving (Show)

data Statement = StFuncall Expr [Expr]
               | StReturn Expr
               deriving (Show)

type Env = Map String Type

emptyEnv :: Env
emptyEnv = Map.empty

top :: Env -> SExpr -> (Env, Function)
top env sexpr =
        case sexpr of
             SList (SAtom "func" : SAtom fnname : SList args : ret : rest) -> func env fnname args ret rest
             _ -> error "top"

resolveBinding :: SExpr -> (String, Type)
resolveBinding (SList [SAtom name, typ]) = (name, resolveType typ)
resolveBinding _ = error "resolveBinding"

resolveType :: SExpr -> Type
resolveType (SAtom "integer") = TInteger
resolveType (SAtom "symbol") = TSymbol
resolveType (SAtom "string") = TString
resolveType _ = error "resolveType"

func :: Env -> String -> [SExpr] -> SExpr -> [SExpr] -> (Env, Function)
func env fnname args ret rest =
        let argBindings = map resolveBinding args
            retType = resolveType ret

            localEnv = foldr (uncurry Map.insert) env argBindings

            body = fst $ foldl' (stmt retType) ([], localEnv) rest

            fn = Function fnname argBindings retType body
            fnt = TFunction (map snd argBindings) retType
        in
        (Map.insert fnname fnt env, fn)

-- put this in Writer maybe
-- I kinda also want Reader for the function context
stmt :: Type -> ([Statement], Env) -> SExpr -> ([Statement], Env)
stmt retType (stmts, env) sexpr =
        case sexpr of
             SList [SAtom "return", e] ->
                     let e' = expr env e in
                     if exprType e' /= retType
                        then error (
                                "return typecheck fail: function has type " ++
                                show retType ++ " but returning " ++ show e' ++
                                " of type "++ show (exprType e'))
                        else (stmts ++ [StReturn $ expr env e], env)
             SList (callee:args) -> (stmts ++ [StFuncall (expr env callee) (map (expr env) args)], env)
             _ -> error "stmt"

expr :: Env -> SExpr -> Expr
expr _ (SInteger i) = EInteger i
expr _ (SSymbol s) = ESymbol s
expr _ (SString s) = EString s
expr env (SAtom a) =
        case Map.lookup a env of
             Just t -> ELookup a t
             Nothing -> error $ "lookup: " ++ a
expr _ _ = error "expr"

exprType :: Expr -> Type
exprType (EInteger _) = TInteger
exprType (ESymbol _) = TSymbol
exprType (EString _) = TString
exprType (ELookup _ t) = t
