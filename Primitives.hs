module Primitives(defaultBindings) where

import Control.Monad.IO.Class (liftIO)
import Data.HashTable (fromList, hashString)

import Evaluate

defaultBindings :: IO Bindings
defaultBindings = sequence
                [fromList hashString
                    [("+", arithLift (+)),
                     ("-", arithLift (-)),
                     ("*", arithLift (*)),
                     ("/", arithLift (/)),


                     ("==", compLift (==)),
                     ("!=", compLift (\x y -> not (x == y))),
                     ("<",  compLift (<)),
                     (">",  compLift (>)),
                     ("<=", compLift (<=)),
                     (">=", compLift (>=)),


                     ("!", logicLift1 (not)),


                     ("print", PrimFun putStrPrim),
                     ("read",  PrimFun getLinePrim)]
                ]

type Number = Double

arithLift :: (Number -> Number -> Number) -> Value
arithLift f =
  PrimFun (\[(NumVal x),(NumVal y)] -> (return . NumVal) (f x y))

compLift :: (Value -> Value -> Bool) -> Value
compLift comp =
  PrimFun (\[a, b] -> (return . BoolVal) (a `comp` b))

logicLift1 :: (Bool -> Bool) -> Value
logicLift1 op =
  PrimFun (\[(BoolVal a)] -> (return . BoolVal) (op a))

putStrPrim :: [Value] -> ProgramEnv Value
putStrPrim [(StrVal str)] = do
           (liftIO . putStr) str
           return Undefined

getLinePrim :: [Value] -> ProgramEnv Value
getLinePrim [] = do
           line <- liftIO getLine
           return (StrVal line)
