module Compiler
    ( Type(..)
    , top
    , Function
    , Env
    , emptyEnv
    ) where

import qualified Data.Map.Strict as Map
import Control.Monad (foldM)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Writer (Writer, execWriter, tell)
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
          | EFuncall Expr [Expr]
          deriving (Show)

data Statement = StEval Expr
               | StReturn Expr
               deriving (Show)

type Env = Map String Type

emptyEnv :: Env
emptyEnv = Map.empty

top :: Env -> SExpr -> (Env, Function)
top env sexpr =
        case sexpr of
             SList (SAtom "func" : SAtom fnName : SList args : ret : rest) -> func env fnName args ret rest
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
func env fnName args ret rest =
        let argBindings = map resolveBinding args
            retType = resolveType ret

            localEnv = foldr (uncurry Map.insert) env argBindings

            body = execWriter $ flip runReaderT (fnName, retType) $ foldM stmt localEnv rest

            fn = Function fnName argBindings retType body
            fnt = TFunction (map snd argBindings) retType
        in
        (Map.insert fnName fnt env, fn)

stmt :: Env -> SExpr -> ReaderT (String, Type) (Writer [Statement]) Env
stmt env sexpr =
        case sexpr of
             SList [SAtom "return", eSexpr] ->
                     let e = expr env eSexpr in
                     ask >>= \(fnName, retType) ->
                     if exprType e /= retType
                        then error (
                                "return typecheck fail: function " ++
                                fnName ++ " has type " ++ show retType ++
                                " but returning " ++ show e ++ " of type " ++
                                show (exprType e))
                        else tell [StReturn e] >> return env
             _ -> tell [StEval (expr env sexpr)] >> return env

expr :: Env -> SExpr -> Expr
expr _ (SInteger i) = EInteger i
expr _ (SSymbol s) = ESymbol s
expr _ (SString s) = EString s
expr env (SAtom a) =
        case Map.lookup a env of
             Just t -> ELookup a t
             Nothing -> error $ "lookup: " ++ a
expr env (SList (calleeSexpr:argSexprs)) =
        let callee = expr env calleeSexpr
            args = map (expr env) argSexprs
        in
        case exprType callee of
             TFunction calleeArgTypes _ ->
                     if length args /= length calleeArgTypes
                        then error (
                                "funcall typecheck fail: argument count mismatch (" ++
                                show (length args) ++ " /= " ++ show (length calleeArgTypes) ++ ")")
                        else let argTypes = map exprType args in
                              if argTypes /= calleeArgTypes
                                 then error (
                                         "funcall typecheck fail: argument type mismatch (" ++
                                         show argTypes ++ " /= " ++ show calleeArgTypes ++ ")")
                                 else EFuncall callee args
             calleeType ->
                     error $ "funcall typecheck fail: callee has type: " ++ show calleeType
expr _ _ = error "expr"

exprType :: Expr -> Type
exprType (EInteger _) = TInteger
exprType (ESymbol _) = TSymbol
exprType (EString _) = TString
exprType (ELookup _ t) = t
exprType (EFuncall callee _) =
        case exprType callee of
             TFunction _ calleeRetType -> calleeRetType
             calleeType ->
                     -- we should never construct an EFuncall to
                     -- a non-TFunction, so it's a bug if this happens.
                     -- how can I put this in the type system without it
                     -- being super awkward?
                     error $ "funcall typecheck fail: callee has type (II): " ++ show calleeType
