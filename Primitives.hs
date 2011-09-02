module Primitives(defaultBindings) where

import Control.Monad.IO.Class (liftIO)
import Data.Map as Map (Map, fromList)

import Evaluate

defaultBindings :: Bindings
defaultBindings = [fromList [("+", arithLift (+)),
                             ("-", arithLift (-)),
                             ("*", arithLift (*)),
                             ("/", arithLift div),
                             ("putStr", PrimFun putStrPrim)
                            ]]

arithLift :: (Integer -> Integer -> Integer) -> Value
arithLift f =
  PrimFun (\[(IntVal x),(IntVal y)] -> (return . IntVal) (f x y))

putStrPrim :: [Value] -> ProgramEnv Value
putStrPrim [(StrVal str)] = do
           (liftIO . putStr) str
           return Undefined

