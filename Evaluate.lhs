\begin{code}
{-# LANGUAGE Rank2Types #-}
module Evaluate (
       ProgramEnv, ProgramState,
       evaluate, apply,
       runProgram, runProgramState, newFrame,
       Value(..), Bindings)
where

import Control.Monad (liftM, msum)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State(StateT, get, put, runStateT, evalStateT)

import Control.Monad.CC (CCT, runCCT, Prompt)

import Data.List (intercalate)
import Data.HashTable as HT
       (HashTable, fromList, lookup, update, insert, hashString)
import Data.IORef

import Parser

data Value = NumVal    Double
           | StrVal    String
           | BoolVal   Bool
           | PromptVal (Prompt () Value)
           | Fun       Args Expr Bindings
           | PrimFun   ([Value] -> ProgramEnv Value)
           | Undefined

type Args         = [Symbol]
type Frame        = HashTable Symbol Value
type Bindings     = [Frame]
type ProgramState = StateT Bindings IO
type ProgramEnv a = forall r. CCT r ProgramState a
type EnvValue     = ProgramEnv Value


instance Show Value where
  show (BoolVal x)    = show x
  show (NumVal  x)    = show x
  show (StrVal  x)    = show x
  show (Fun args _ _) = "<function(" ++ (intercalate "," args) ++ ")>"
  show (PrimFun _)    = "<primitive function>"
  show Undefined      = "<undefined>"

instance Eq Value where
  BoolVal a == BoolVal b   =   a == b
  NumVal  a == NumVal  b   =   a == b
  StrVal  a == StrVal  b   =   a == b
  _         == _           =   False

instance Ord Value where
  compare (BoolVal a) (BoolVal b)  =  compare a b
  compare (NumVal  a) (NumVal  b)  =  compare a b
  compare (StrVal  a) (StrVal  b)  =  compare a b
  -- TODO create new ord class for partial orderings
  compare _ _ = undefined

runProgram :: IO Bindings -> ProgramEnv () -> IO ()
runProgram getBindings program = do
           bindings <- getBindings
           evalStateT (runCCT program) bindings
           return ()

evaluate :: Expr -> ProgramEnv Value

evaluate (BoolLit b) = return (BoolVal b)
evaluate (NumLit  i) = return (NumVal  i)
evaluate (StrLit  s) = return (StrVal  s)
evaluate (Ident  id) = binding id

evaluate (UniOp op exp) = do
         fun <- binding op
         val <- evaluate exp
         apply fun [val]

evaluate (BinOp "=" (Ident name) r) = do
         rhs <- evaluate r
         rebind name rhs

evaluate (BinOp op l r) = do
         fun <- binding op
         lhs <- evaluate l
         rhs <- evaluate r
         apply fun [lhs, rhs]

evaluate (Multi exps) = (liftM last . mapM evaluate) exps

evaluate (Lambda args exp) = withCurrentBindings (Fun args exp)

evaluate (Application fun exps) = do
         fun'  <- evaluate fun
         exps' <- mapM evaluate exps
         apply fun' exps'

evaluate (If test then' melse') = do
         val <- evaluate test
         case val of
           BoolVal b -> case b of
             True  -> evaluate then'
             False -> case melse' of
               Just else' -> evaluate else'
               Nothing    -> return Undefined

apply :: Value -> [Value] -> ProgramEnv Value
apply (PrimFun f) exps = f exps

apply (Fun args body closure) exps = do
      frame <- newFrame (zip args exps)
      withBindings (frame : closure) (evaluate body)



get' :: ProgramEnv Bindings
put' :: Bindings -> ProgramEnv ()

get'   = lift get
put' a = lift (put a)

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
        bindings <- get'
        mvals    <- mapM (lookup' sym) bindings
        let mval = msum mvals
        case mval of
             Just val -> return val
             Nothing  -> return Undefined

rebind :: Symbol -> Value -> ProgramEnv Value
rebind sym val = do
       bindings <- get'
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

withBindings :: Bindings -> ProgramEnv a -> ProgramEnv a
withBindings bindings computation = do
           base <- get'
           put' bindings
           val <- computation
           put' base
           return val

withCurrentBindings :: (Bindings -> a) -> ProgramEnv a
withCurrentBindings f = do
           bindings <- get'
           return (f bindings)

runProgramState :: IO Bindings -> ProgramState () -> IO ()
runProgramState getBindings program = do
                bindings <- getBindings
                evalStateT program bindings
                return ()

evaluateIO :: Expr -> Bindings -> IO Value
evaluateIO exp env = do
           value <- evalStateT (runCCT (evaluate exp)) env
           case value of
             PromptVal _ -> return Undefined
             _           -> return value

\end{code}
