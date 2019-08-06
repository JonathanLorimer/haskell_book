module LearnParsersExercises where

import Text.Trifecta
import Text.Parser.Combinators

one' :: Parser ()
one' = char '1' >> eof

oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

-- e1' = testParse $ oneTwo >> eof