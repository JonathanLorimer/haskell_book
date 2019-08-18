module MonadTrans where

import Control.Monad
import Control.Monad.Trans.Class
import EitherT
import StateT

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift = StateT . (\a s -> (,) <$> a <*> (pure s))
