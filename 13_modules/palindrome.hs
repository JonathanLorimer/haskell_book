
import           Control.Monad
import           System.Exit
import           Data.Char                      ( toLower
                                                , isAlpha
                                                )

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let line = map toLower $ filter isAlpha line1
    case (line == reverse line) of
        True  -> putStrLn "It's a palindrome!"
        False -> do
            putStrLn "Failure"
            return exitSuccess ()
