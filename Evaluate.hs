{-# LANGUAGE FlexibleInstances #-}
module Evaluate(evaluate) where

import Data.Map

import Parser

data Value = IntVal Integer
           | StrVal String
           | PrimFun ([Value] -> Value)
  deriving (Show, Eq)

instance Show ([Value] -> Value) where
  show _  = "<primitive function>"

instance Eq ([Value] -> Value) where
  x == y  = False

type Bindings = Map String Value

primLift  f = PrimFun (\[(IntVal x),(IntVal y)] -> IntVal (f x y))

bindings :: Bindings
bindings = fromList [("+", primLift (+)),
                     ("-", primLift (-)),
                     ("*", primLift (*)),
                     ("/", primLift div),
                     ("x", IntVal 3)]

evaluate :: Expr -> Value
evaluate (IntLit i) = IntVal i
evaluate (StrLit s) = StrVal s
evaluate (Ident id) = bindings ! id
evaluate (BinOp op l r) =
         case (bindings ! op) of
              PrimFun f -> f [(evaluate l), (evaluate r)]
              _ -> IntVal 0
evaluate _ = IntVal 0
