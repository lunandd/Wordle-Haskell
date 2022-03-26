module Main where

import Data.Char (toLower)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import Wordle.State
import Wordle.Validation (checkGuess)
import Wordle.Word (isWordValid, randomWord)

gameLoop :: String -> Int -> IO ()
gameLoop randomWord guessesLeft = do
  putStrLn $ "Enter your guess [" ++ show guessesLeft ++ "/6]"
  putStr "> "
  guessed <- getLine
  isValid <- isWordValid guessed
  if isValid
    then do
      let state =
            GameState
              { word = randomWord,
                guess = guessed
              }
      putStrLn $ concatMap show (checkGuess state)
      if guessesLeft < 6 && word state /= guess state
        then do
          gameLoop randomWord (guessesLeft + 1)
        else putStrLn $ "The word is " ++ word state
    else do
      putStrLn "Invalid word entered"
      gameLoop randomWord guessesLeft

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  gameLoop word 1