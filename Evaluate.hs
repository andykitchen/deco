{-# LANGUAGE FlexibleInstances #-}
module Evaluate (
       ProgramEnv, evaluate, runProgram, newFrame,
       Value(..), Bindings)
where

import Control.Monad (liftM, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State(StateT, get, put, runStateT, evalStateT)

import Data.List (intercalate)
import Data.HashTable as HT
       (HashTable, fromList, lookup, update, insert, hashString)

import Parser

data Value = NumVal Double
           | StrVal String
           | Fun [Symbol] Expr Bindings
           | PrimFun ([Value] -> ProgramEnv Value)
           | Undefined

instance Show Value where
  show (NumVal x)     = show x
  show (StrVal x)     = show x
  show (Fun args _ _) = "<function(" ++ (intercalate "," args) ++ ")>"
  show (PrimFun _)    = "<primitive function>"
  show Undefined      = "<undefined>"

type Frame      = HashTable Symbol Value
type Bindings   = [Frame]
type ProgramEnv = StateT Bindings IO
type EnvValue   = ProgramEnv Value

newFrame :: [(Symbol, Value)] -> ProgramEnv Frame
lookup'  :: Symbol -> Frame -> ProgramEnv (Maybe Value)
update'  :: Frame -> Symbol -> Value -> ProgramEnv Bool
insert'  :: Frame -> Symbol -> Value -> ProgramEnv ()

newFrame ls           = liftIO (HT.fromList HT.hashString ls)
lookup' sym frame     = liftIO (HT.lookup frame sym)
update' frame sym val = liftIO (HT.update frame sym val)
insert' frame sym val = liftIO (HT.insert frame sym val)

binding :: Symbol -> ProgramEnv Value
binding sym = do
        bindings <- get
        mvals    <- mapM (lookup' sym) bindings
        let mval = msum mvals
        case mval of
             Just val -> return val
             Nothing  -> return Undefined

rebind :: Symbol -> Value -> ProgramEnv Value
rebind sym val = do
       bindings <- get
       bound <- rebindWalk sym val bindings
       case bound of
            False -> insert' (head bindings) sym val
            True  -> return ()
       return val

rebindWalk :: Symbol -> Value -> Bindings -> ProgramEnv Bool
rebindWalk sym val [] = return False
rebindWalk sym val (frame : stack) = do
           found <- update' frame sym val
           case found of
                True  -> return True
                False -> rebindWalk sym val stack

stackframe :: Bindings -> ProgramEnv a -> ProgramEnv a
stackframe frame computation = do
           base <- get
           put frame
           val <- computation
           put base
           return val

withBinding :: (Bindings -> a) -> ProgramEnv a
withBinding f = do
            bindings <- get
            return (f bindings)

runProgram :: IO Bindings -> ProgramEnv a -> IO a
runProgram getBindings program = do
           bindings <- getBindings
           evalStateT program bindings

evaluateIO :: Expr -> Bindings -> IO (Value, Bindings)
evaluateIO exp env = runStateT (evaluate exp) env

evaluate :: Expr -> ProgramEnv Value

evaluate (NumLit i) = return (NumVal i)
evaluate (StrLit s) = return (StrVal s)
evaluate (Ident id) = binding id

evaluate (BinOp "=" (Ident name) r) = do
         rhs <- evaluate r
         rebind name rhs

evaluate (BinOp op l r) = do
         var <- binding op
         lhs <- evaluate l
         rhs <- evaluate r
         apply var [lhs, rhs]

evaluate (Multi exps) = (liftM last . mapM evaluate) exps

evaluate (Lambda args exp) = withBinding (Fun args exp)

evaluate (Application fun exps) = do
         fun'  <- evaluate fun
         exps' <- mapM evaluate exps
         apply fun' exps'

evaluate _ = return Undefined

apply :: Value -> [Value] -> ProgramEnv Value
apply (PrimFun f) exps = f exps

apply (Fun args body closure) exps = do
      frame <- newFrame (zip args exps)
      stackframe (frame : closure) (evaluate body)
