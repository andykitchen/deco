module Utility where

import System.Console.Haskeline.MonadException
import Control.Exception ()
import Prelude hiding (catch)

import Control.Monad.Trans.State.Strict as Strict
       (StateT(..), runStateT, mapStateT)

instance MonadException m => MonadException (Strict.StateT s m) where
    catch f h = StateT $ \s -> catch (runStateT f s)
                                (\e -> runStateT (h e) s)
    block     = mapStateT block
    unblock   = mapStateT unblock
