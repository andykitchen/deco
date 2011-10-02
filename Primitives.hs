{-# LANGUAGE Rank2Types #-}
module Primitives(defaultBindings) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.CC

import Data.HashTable (fromList, hashString)

import Evaluate

import Unsafe.Coerce

defaultBindings :: IO Bindings
defaultBindings = sequence
                [fromList hashString
                    [("+", PrimFun plusPrim),
                     ("-", arithLift (-)),
                     ("*", arithLift (*)),
                     ("/", arithLift (/)),


                     ("==", compLift (==)),
                     ("!=", compLift (/=)),
                     ("<",  compLift (<)),
                     (">",  compLift (>)),
                     ("<=", compLift (<=)),
                     (">=", compLift (>=)),

                     ("!",  logicLift1 not),
                     ("&&", logicLift  (&&)),
                     ("||", logicLift  (||)),

                     ("shift", PrimFun shiftPrim),
                     ("reset", PrimFun resetPrim),

                     ("string", PrimFun showPrim),

                     ("print", PrimFun putStrPrim),
                     ("read",  PrimFun getLinePrim)]
                ]

type Number = Double
type Prim = [Value] -> ProgramEnv Value

plusPrim    :: Prim
putStrPrim  :: Prim
getLinePrim :: Prim
showPrim    :: Prim
shiftPrim   :: Prim
resetPrim   :: Prim

plusPrim [NumVal x, NumVal y] = (return . NumVal) (x +  y)
plusPrim [StrVal x, StrVal y] = (return . StrVal) (x ++ y)

putStrPrim [StrVal str] = do
           (liftIO . putStr) str
           return Undefined
putStrPrim [NumVal  num]  = print' num
putStrPrim [BoolVal bool] = print' bool

print' val = (liftIO . print) val >> return Undefined

getLinePrim [] = do
           line <- liftIO getLine
           return (StrVal line)

showPrim [NumVal  v] = (return . StrVal . show) v
showPrim [BoolVal v] = (return . StrVal . show) v
showPrim _ = return Undefined

unsafeCoercePrompt :: Prompt ans a -> Prompt ans' a
unsafeCoercePrompt = unsafeCoerce
unsafeCoercePrim :: ([Value] -> CCT ans m a) -> ([Value] -> CCT ans' m a)
unsafeCoercePrim = unsafeCoerce
{-
        Proof sketch for why this is actually safe: right now only
        runProgram and runProgramState are the only exported functions
        that can unwrap a ProgramEnv monad and they prevent the
        bindings from escaping which means that the stored promts
        cannot be use outside of a context they were created in. This
        property is very akward to prove using the Haskell type system
        so we go outside of it.

        The phantom type 'ans' is used to prevent prompts from
        escaping the contexts they are created in, we override this
        check by using unsafeCoerce wrapped up to make it slightly
        more safe.

        As far as I can gather from the GHC documentation because
        'ans' is an uninstantiated type or phantom type all promts
        should be representation compatible.
-}

shiftPrim [PromptVal p, f@(Fun _ _ _)] =
            shift (unsafeCoercePrompt p) $ \k ->
              let k' [val] = k (return val) in
              apply f [PrimFun (unsafeCoercePrim k')]

resetPrim [f@(Fun _ _ _)] =
          reset $ \p -> apply f [PromptVal (unsafeCoercePrompt p)]

arithLift :: (Number -> Number -> Number) -> Value
arithLift f =
  PrimFun (\[NumVal x, NumVal y] -> (return . NumVal) (f x y))

compLift :: (Value -> Value -> Bool) -> Value
compLift comp =
  PrimFun (\[a, b] -> (return . BoolVal) (a `comp` b))

logicLift1 :: (Bool -> Bool) -> Value
logicLift1 op =
  PrimFun (\[BoolVal a] -> (return . BoolVal) (op a))

logicLift :: (Bool -> Bool -> Bool) -> Value
logicLift op =
  PrimFun (\[BoolVal a, BoolVal b] -> (return . BoolVal) (a `op` b))

