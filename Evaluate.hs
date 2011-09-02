{-# LANGUAGE FlexibleInstances #-}
module Evaluate(ProgramEnv, evaluate, runProgram, defaultBindings) where

import Control.Monad (liftM, msum)
import Control.Monad.Trans.State(StateT, get, put, runStateT, evalStateT)
import Control.Monad.IO.Class (MonadIO)
import Data.Map as Map (Map, fromList, insert, lookup)

import Parser

data Value = IntVal Integer
           | StrVal String
           | Fun [Symbol] Expr Bindings
           | PrimFun ([Value] -> Value)
           | Undefined
  deriving (Show, Eq)

instance Show ([Value] -> Value) where
  show _  = "<primitive function>"

instance Eq ([Value] -> Value) where
  x == y  = False

type Bindings   = [Map Symbol Value]
type ProgramEnv = StateT Bindings IO

primArithLift :: (Integer -> Integer -> Integer) -> Value
primArithLift  f = PrimFun (\[(IntVal x),(IntVal y)] -> IntVal (f x y))

defaultBindings :: Bindings
defaultBindings = [fromList [("+", primArithLift (+)),
                            ("-", primArithLift (-)),
                            ("*", primArithLift (*)),
                            ("/", primArithLift div)
                            ]]

binding :: String -> ProgramEnv Value
binding sym = do
        bindings <- get
        let vals = map (Map.lookup sym) bindings
            mval = msum vals
        case mval of
             Just val -> return val
             Nothing  -> return Undefined

rebind :: String -> Value -> ProgramEnv Value
rebind str val = do
       cur : stack <- get
       put (insert str val cur : stack)
       return val

stackframe :: Map Symbol Value -> Bindings -> ProgramEnv Value -> ProgramEnv Value
stackframe frame closure computation = do
           base <- get
           put (frame : closure)
           val <- computation
           put base
           return val

runProgram :: Bindings -> ProgramEnv a -> IO a
runProgram = flip evalStateT

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

evaluate (Multi exps) = (liftM last . mapM evaluate) exps

evaluate (Lambda args exp) = do
         binds <- get -- TODO factor out
         return (Fun args exp binds)

evaluate (Application fun exps) = do
         fun'  <- evaluate fun
         exps' <- mapM evaluate exps
         case fun' of
              Fun args body closure -> apply args body closure exps'

evaluate _ = return Undefined

apply :: [Symbol] -> Expr -> Bindings -> [Value] -> ProgramEnv Value
apply args body closure exps = stackframe frame closure (evaluate body)
      where frame = fromList (zip args exps)
