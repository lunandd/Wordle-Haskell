module Main where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Test.QuickCheck (elements, generate)
import Wordle.Logic

game :: String -> String -> IORef Int -> IO ()
game randomWord fileContents guesses = do
  guessesLeft <- readIORef guesses
  input <- getLine
  let guess = fmap toLower (take 5 input)
  if guess `elem` lines fileContents
    then do
      ref <- newIORef []
      let state =
            GameState
              { word = randomWord,
                foundLetters = ref,
                guess = guess
              }
      let checked = checkGuess state
      writeIORef ref checked
      formatCG checked
      if guessesLeft < 6
        then do
          modifyIORef guesses (+ 1)
          game randomWord fileContents guesses
        else putStrLn $ "The word is " ++ word state
    else do
      putStrLn "Invalid word entered"
      game randomWord fileContents guesses

main :: IO ()
main = do
  guesses <- newIORef 1
  fileContents <- readFile "words.txt"
  randomWord <- generate $ elements $ lines fileContents
  putStrLn "Enter a 5 letter word"
  game randomWord fileContents guesses