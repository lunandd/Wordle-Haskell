{-# LANGUAGE LambdaCase #-}

module Wordle.Logic (GameState (..), Checked, checkGuess, formatCG) where

import Control.Monad
import Data.List (elemIndex, intercalate, intersperse)
import Data.Maybe (isJust, isNothing)
import GHC.IORef (IORef)
import System.Console.ANSI

data Checked a = CorrectSpot a | WrongSpot a | NonExistent a deriving (Show, Eq)

data GameState = GameState
  { word :: String,
    foundLetters :: IORef [Checked Char],
    guess :: String
  }

checkChar :: GameState -> Char -> Checked Char
checkChar state c
  | indexInWord == indexInGuess = CorrectSpot c
  | isJust indexInWord && indexInWord /= indexInGuess = WrongSpot c
  | isNothing indexInWord = NonExistent c
  where
    indexInWord = elemIndex c $ word state
    indexInGuess = elemIndex c $ guess state

checkGuess :: GameState -> [Checked Char]
checkGuess state = fmap (checkChar state) (guess state)

formatCG :: [Checked Char] -> IO ()
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