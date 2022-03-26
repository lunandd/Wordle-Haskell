module Wordle.Validation (checkGuess, Checked) where

import Data.List (elemIndex)
import Data.Maybe (isJust)
import Wordle.State (Checked (..), GameState (guess, word))

checkChar :: GameState -> Char -> Checked
checkChar state c
  | isInWord && indexInGuess == indexInWord = CGreen c
  | isInWord && indexInGuess /= indexInWord = CYellow c
  | not isInWord = CRed c
  where
    indexInWord = elemIndex c $ word state
    indexInGuess = elemIndex c $ guess state
    isInWord = elem c $ word state

checkGuess :: GameState -> [Checked]
checkGuess state = fmap (checkChar state) (guess state)