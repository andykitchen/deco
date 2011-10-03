module Utility where

import System.Console.Haskeline.MonadException
import Control.Exception ()
import Prelude hiding (catch)

import Unsafe.Coerce

import Control.Monad.Trans.State.Strict as Strict
       (StateT(..), runStateT, mapStateT)

import Control.Monad.Trans.Class (lift)
import Control.Monad.CC

instance MonadException m => MonadException (Strict.StateT s m) where
    catch f h = StateT $ \s -> catch (runStateT f s)
                                (\e -> runStateT (h e) s)
    block     = mapStateT block
    unblock   = mapStateT unblock

-- class MonadIO m => MonadException m where
--     catch :: Exception e => m a -> (e -> m a) -> m a
--     block :: m a -> m a
--     unblock :: m a -> m a

unsafeCoerceCCT :: Monad m => CCT ans m a -> CCT ans' m a
unsafeCoerceCCT = unsafeCoerce

instance MonadException m => MonadException (CCT ans m) where
    catch f h = lift $ catch (runCCT (unsafeCoerceCCT f))
                               (\e -> runCCT (unsafeCoerceCCT (h e)))
    block m   = lift $ unblock $ runCCT $ unsafeCoerceCCT m
    unblock m = lift $ block   $ runCCT $ unsafeCoerceCCT m
