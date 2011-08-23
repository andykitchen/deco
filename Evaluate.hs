{-# LANGUAGE FlexibleInstances #-}
module Evaluate(evaluateIO, Bindings, defaultBindings) where

import Control.Monad.Trans.State
import Control.Monad.IO.Class (MonadIO)
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

type Bindings   = Map String Value
type ProgramEnv = StateT Bindings IO

primLift :: (Integer -> Integer -> Integer) -> Value
primLift  f = PrimFun (\[(IntVal x),(IntVal y)] -> IntVal (f x y))

defaultBindings :: Bindings
defaultBindings = fromList [("+", primLift (+)),
                            ("-", primLift (-)),
                            ("*", primLift (*)),
                            ("/", primLift div),
                            ("x", IntVal 3)]

binding :: String -> ProgramEnv Value
binding str = do
        bindings <- get
        return (bindings ! str)

rebind :: String -> Value -> ProgramEnv Value
rebind str val = do
       bindings <- get
       put (insert str val bindings)
       return val

evaluateIO :: Expr -> Bindings -> IO (Value, Bindings)
evaluateIO exp env = runStateT (evaluate exp) env

evaluate :: Expr -> ProgramEnv Value

evaluate (IntLit i) = return (IntVal i)
evaluate (StrLit s) = return (StrVal s)
evaluate (Ident id) = binding id

evaluate (BinOp "=" (Ident name) r) = do
         rhs <- evaluate r
         rebind name rhs

evaluate (BinOp op l r) = do
         var <- binding op
         lhs <- evaluate l
         rhs <- evaluate r
         case var of
              PrimFun f -> return (f [lhs, rhs])

evaluate _ = return (IntVal 0)
