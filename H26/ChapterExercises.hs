module ChapterExercises where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad

-- 1 and 2
rDec :: Num a => Reader a a
rDec = asks $ flip (-) 1

-- 3 and 4
rShow :: Show a => ReaderT a Identity String
rShow = asks show

-- 5
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn ("Hi: " ++ show r)
  pure (r + 1)

-- 6
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  putStrLn ("Hi: " ++ show s)
  pure (show s, s + 1)

-- Fix the code
isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExite :: MaybeT IO String
maybeExite = MaybeT $ do
  v <- getLine
  if isValid v
    then pure (Just v)
    else pure Nothing

doExcite :: IO ()
doExcite = do
  putStrLn "Say something exciting!"
  exite <- runMaybeT maybeExite
  case exite of
    Nothing -> putStrLn "MOAR EXITING"
    Just e -> putStrLn ("Good, very exiting: " ++ e)

--
