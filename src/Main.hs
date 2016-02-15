module Main where

import qualified Data.Map.Strict as Map
import           Data.Maybe


newtype Context = Context {unctx :: Map.Map String Expr}


type Form = Either Definition Expr
data Definition = Definition String Expr

data Expr = Int Integer
        | Symbol String
        | List [Expr]
        | Builtin Prim
        | Fn (Context->[Expr]->(Context, Expr))

data Prim = Add | Sub | Mul

eval :: Form -> Context -> (Context, Maybe Expr)
eval form ctx = case form of
    Left def -> (evalDef def ctx, Nothing )
    Right expr -> (ctx, Just $ evalExpr ctx expr)

evalDef :: Definition -> Context -> Context
evalDef (Definition sym expr) (Context ctx) = Context ( Map.insert sym expr ctx)

evalExpr :: Context -> Expr -> Expr
evalExpr ctx (Int v) = Int v
evalExpr ctx (Symbol s) = evalExpr ctx (fromMaybe nil $ Map.lookup s (unctx ctx))
evalExpr ctx (List []) = nil
evalExpr ctx (List (x:xs)) = case x of
    Builtin Add -> Int (sum (map ((\(Int x) -> x ) . evalExpr ctx) xs))
    Builtin Sub -> Int (foldl (-)
                            ( (\(Int x) -> x ) $ evalExpr ctx (head xs) )
                            (map ( (\(Int x) -> x ) . evalExpr ctx) (tail xs) ))
    Builtin Mul -> Int (product (map ((\(Int x) -> x ) . evalExpr ctx) xs))

nil :: Expr
nil = List []

main :: IO ()
main = do
  putStrLn "hello world"
