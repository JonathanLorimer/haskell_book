module LearnParsers where

import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"