\ignore{
\begin{code}
{-# LANGUAGE Rank2Types #-}
module Evaluate (
         ProgramEnv, Value(..), Bindings,
         runProgram, evaluate, apply
       )
where

import Control.Monad (liftM, msum)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State(StateT, get, put, runStateT, evalStateT)

import Control.Monad.CC (CCT, runCCT, Prompt)

import Data.List (intercalate)
import Data.HashTable as HT
       (HashTable, fromList, lookup, update, insert, hashString)

import Parser
\end{code}
}

\subsection{Values}

We use ADT for the value type in our nested language:

\vspace{5 mm}
\begin{code}
data Value = NumVal    Double
           | StrVal    String
           | BoolVal   Bool
           | Fun       Args Expr Bindings
           | PrimFun   ([Value] -> ProgramEnv Value)
           | PromptVal (Prompt () Value)
           | Undefined

type Args     = [Symbol]
type Bindings = [Frame]

type Frame    = HashTable Symbol Value
\end{code}

First we have three simple constructors for primtive values.

\type{Fun} is for functions defined inside the language.  The
constructor fields are the list of argument names, the body of the
function and the static link that records the function's closure.  In
this implementation we use a spaghetti-stack to represent the lexical
environment of a function. Because in our language all values are
reference types, we use a stack of \type{Hashtable} which is a
by-reference data structure that can only be accessed inside the
\type{IO} monad.

\type{PrimFun} represents a primitive function that is implemented in
haskell, because the language is very dynamic, all functions are
varardic so a primtive function takes a list of values and importantly
returns a value inside the \type{ProgramEnv} monad. Which is discussed
slightly later.

\type{PromptVal} is rather special, it is used to represented delimited
continuation prompts. Prompts are discussed with more detail in section
\ref{discussion:delcont}.

Finally we have an \type{Undefined} constructor to represent undefined
values within the language as distinct from haskell's own concept of
undefined values.

\vspace{5 mm} % force page break
\subsection{The Program Environment Monad}

\begin{code}
type ProgramEnv a = forall ans. CCT ans ProgramState a
type ProgramState = StateT Bindings IO
\end{code}

The program environment monad (\type{ProgramEnv}) is the main vehicle
for representing the impure evaluator cleanly in haskell. It therefore
has a lot of functionality. Fortunatly its complexity is managable
because it is built out of well seperated components.

The \type{ProgramEnv} monad is built by composing monad transformers,
which are covered in more depth in section \ref{discussion:monadtrans}.  There
are two transformers layered on our stack the \type{StateT} monad and
the \type{CCT} monad.

\begin{itemize}

\item The \type{IO} monad forms the base of our stack. It is required
  for communication with the outside world.

\item The \type{StateT} transformer allows us to read and write a global shared
  state. This state is used to hold the current evaluation scope used
  for variable lookup.

\item The \type{CCT} transformer is the delimited continuation monad
  transformer, by running our \function{evaluate} function under this
  transformer we can supply primitives to the interpreted language
  that allow it to manipulate the control flow of its evaluator and so
  therefore its own control flow.

\end{itemize}


\subsubsection{Unwinding the Program Monad}
The \function{runProgram} function unwinds the \type{ProgramEnv} monad
stack to yield an \type{IO} `action' that represents the execution of
a given program.

\begin{code}
runProgram :: IO Bindings -> ProgramEnv () -> IO ()
runProgram getBindings program = do
           bindings <- getBindings
           evalStateT (runCCT program) bindings
           return ()
\end{code}

One can see \function{evalStateT} takes an extra argument when
unwinding the \type{StateT} transformer which is the initial state, it
is used to supply the initial variable bindings (see Primitives module
in source code).

The other thing to note is the expanded type of \function{runProgram}
once it has already been partially applied to a binding:

\begin{code}%
        (forall ans. CCT ans ProgramState ()) -> IO ()
\end{code}

This is a rank-2 type because the quantifier is limited in scope to
the left hand side of the arrow. The use of a uninstantiated type
variable to prevent values escaping a monadic context is covered in
section \ref{discussion:rank2}.

Also, interpreting the type, we can see that the function operates on
monads wrapped around the unit type; values of which convey no
information.  This fits with out intuitive understanding, because it
means that the only non-trivial functions of this type operate on the
monaic wrapping only. This is an interesting part of the correctness of
the Deco code, discussed more in section \ref{discussion:unsafecast}.

\subsection{The Evaluation Function}

The evaluation functions is straight forward in it's implementation:
deconstructing the program's abstract syntax tree and producing appropriate
actions inside the \type{ProgramEnv} monad.

\begin{code}
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
\end{code}


\ignore{
\begin{code}
-- Utility functions

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

evaluateIO :: Expr -> Bindings -> IO Value
evaluateIO exp env = do
           value <- evalStateT (runCCT (evaluate exp)) env
           case value of
             PromptVal _ -> return Undefined
             _           -> return value

-- Instance declerations for Value

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
\end{code}
}
