module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLenght :: Int
minWordLenght = 5

maxWordLenght :: Int
maxWordLenght = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLenght aw)
  where gameLenght w =
          let l = length (w :: String)
          in l >= minWordLenght && l < maxWordLenght

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
    ++ " Wrong guesses so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _) c = c `elem` s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s) c = c `elem` s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

-- Takes a Puzzle and a Char and returns an NEW (updated) Puzzle
fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c
      | c `elem` word = Puzzle word newFilledInSoFar s
      | otherwise = Puzzle word filledInSoFar (c : s)
          where newFilledInSoFar = zipWith (zipper c) word filledInSoFar
                zipper c wordElem filledInSoFarElem =
                  if c == wordElem then Just wordElem else filledInSoFarElem
{-
in addition to the code in the Haskellbook I added the guards to
check if the guessed Char (c) is in the word and only add Wrong
guesses in the list (s). This is to only count wrong guesses towards
the losing conditions.
I've also changed the order of the helper functions in 'where' and
the parameter names to the zipper function, because I found this one
difficult in the book.
zipWith takes a 2 argument function a -> b -> c and two lists
[a] and [b] form which each element a and b are zipped to [c]
using the functions a -> b -> c.
But 'zipper' has three arguments. Turns out that by using 'zipper c'
in zipWith you can add c as the first argument, followed by an itteration
of the elemets from [a] and list [b], here [a] is word (String = [Char])
and [b] is filledInSoFar ([Maybe Char]). This allows for the test of
equality between argument c and the elemets of word (which I named
wordElem) and to return Just wordElem or the elemtent from
filledInSoFar (which I named filledInSoFarElem)
-}

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess =
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True ) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn $ "Good guess, filling the positions of " ++ [guess]
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Wrong guess, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length guessed) > 7 then do
    putStrLn $ "You lose! The word was: " ++ wordToGuess
    exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then do
    putStrLn "You win!"
    exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  hSetBuffering stdout NoBuffering
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
