module Primitives(defaultBindings) where

import Control.Monad.IO.Class (liftIO)
import Data.Map as Map (Map, fromList)

import Evaluate

defaultBindings :: Bindings
defaultBindings = [fromList [("+", arithLift (+)),
                             ("-", arithLift (-)),
                             ("*", arithLift (*)),
                             ("/", arithLift (/)),
                             ("print", PrimFun putStrPrim)
                            ]]

arithLift f =
  PrimFun (\[(NumVal x),(NumVal y)] -> (return . NumVal) (f x y))

putStrPrim :: [Value] -> ProgramEnv Value
putStrPrim [(StrVal str)] = do
           (liftIO . putStr) str
           return Undefined

