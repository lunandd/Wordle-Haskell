module Wordle.Word where

import Control.Monad (forM_)
import Data.Foldable (find)
import System.Random (randomRIO)

wordList :: IO [String]
wordList = lines <$> readFile "wordList.txt"

randomWord :: IO String
randomWord = do
  list <- wordList
  randIndex <- randomRIO (0, length list - 1)
  return $ list !! randIndex

isWordValid :: String -> IO Bool
isWordValid word = elem word <$> wordList