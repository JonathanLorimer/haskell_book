module Ask where

import Control.Monad.Reader

ask :: Reader a a
ask = reader id