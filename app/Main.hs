module Main where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.IORef (newIORef, writeIORef)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Test.QuickCheck (elements, generate)
import Wordle.Logic

main :: IO ()
main = do
  fileContents <- readFile "wordle-nyt-answers.txt"
  randomWord <- generate $ elements $ lines fileContents
  putStrLn "Enter a 5 letter word"
  guess <- forM [1 .. 5] $ const getChar
  let lowercaseGuess = fmap toLower guess
  if lowercaseGuess `elem` lines fileContents
    then do
      ref <- newIORef []
      let state =
            GameState
              { word = randomWord,
                foundLetters = ref,
                guess = lowercaseGuess
              }
      let checked = checkGuess state
      writeIORef ref checked
      putStr "You guessed: "
      formatCG checked
      putStrLn $ "The word is " ++ word state
    else do
      putStrLn "Invalid word entered"
      main