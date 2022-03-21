{-# LANGUAGE LambdaCase #-}

module Wordle.Logic (GameState (..), Letter, checkGuess, formatCG) where

import Control.Monad
import Data.List (elemIndex, intercalate, intersperse)
import Data.Maybe (isJust, isNothing)
import GHC.IORef (IORef)
import System.Console.ANSI

type Guess = [Char]

type CheckedGuess = [Letter Char]

data Letter a = CorrectSpot a | WrongSpot a | NonExistent a deriving (Show, Eq)

data GameState = GameState
  { word :: String,
    foundLetters :: IORef [Letter Char],
    guess :: Guess
  }

charToLetter :: GameState -> Char -> Letter Char
charToLetter state c
  | indexInWord == indexInGuess = CorrectSpot c
  | isJust indexInWord && indexInWord /= indexInGuess = WrongSpot c
  | isNothing indexInWord = NonExistent c
  where
    indexInWord = elemIndex c $ word state
    indexInGuess = elemIndex c $ guess state

checkGuess :: GameState -> CheckedGuess
checkGuess state = fmap (charToLetter state) (guess state)

formatCG :: CheckedGuess -> IO ()
formatCG guess =
  forM_
    guess
    ( \case
        CorrectSpot a ->
          setSGR [SetColor Foreground Vivid White, SetColor Background Dull Green]
            >> putChar a
            >> setSGR [Reset]
        WrongSpot a ->
          setSGR [SetColor Foreground Vivid White, SetColor Background Dull Yellow]
            >> putChar a
            >> setSGR [Reset]
        NonExistent a ->
          putChar a
    )
    >> putStrLn ""