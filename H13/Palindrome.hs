module Palindrome where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  let line2 = foldr ((:) . toLower) [] (filter isLetter line1) in
    case (line2 == reverse line2)  of
      True -> putStrLn "it's a palindrome!"
      False -> do
        putStrLn "Not a palindrome."
        exitSuccess
