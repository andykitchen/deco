module Primitives(defaultBindings) where

import Control.Monad.IO.Class (liftIO)
import Data.HashTable (fromList, hashString)

import Evaluate

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

                     ("string", PrimFun showPrim),

                     ("print", PrimFun putStrPrim),
                     ("read",  PrimFun getLinePrim)]
                ]

type Number = Double

arithLift :: (Number -> Number -> Number) -> Value
arithLift f =
  PrimFun (\[(NumVal x),(NumVal y)] -> (return . NumVal) (f x y))

plusPrim [(NumVal x),(NumVal y)] = (return . NumVal) (x +  y)
plusPrim [(StrVal x),(StrVal y)] = (return . StrVal) (x ++ y)

compLift :: (Value -> Value -> Bool) -> Value
compLift comp =
  PrimFun (\[a, b] -> (return . BoolVal) (a `comp` b))

logicLift1 :: (Bool -> Bool) -> Value
logicLift1 op =
  PrimFun (\[(BoolVal a)] -> (return . BoolVal) (op a))

logicLift :: (Bool -> Bool -> Bool) -> Value
logicLift op =
  PrimFun (\[(BoolVal a), (BoolVal b)] -> (return . BoolVal) (a `op` b))

putStrPrim :: [Value] -> ProgramEnv Value
putStrPrim [(StrVal str)] = do
           (liftIO . putStr) str
           return Undefined
putStrPrim [(NumVal  num)]  = print' num
putStrPrim [(BoolVal bool)] = print' bool

print' val = (liftIO . print) val >> return Undefined

getLinePrim :: [Value] -> ProgramEnv Value
getLinePrim [] = do
           line <- liftIO getLine
           return (StrVal line)

showPrim :: [Value] -> ProgramEnv Value
showPrim [(NumVal  v)] = (return . StrVal . show) v
showPrim [(BoolVal v)] = (return . StrVal . show) v
showPrim _ = return Undefined