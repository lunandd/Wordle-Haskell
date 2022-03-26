{-# LANGUAGE LambdaCase #-}

module Wordle.State where

import System.Console.Pretty

data GameState = GameState
  { word :: String,
    guess :: String
  }

data Checked = CGreen Char | CYellow Char | CRed Char

instance Show Checked where
  show = \case
    CGreen a -> style Bold . bgColor Green $ [a]
    CYellow a -> style Bold . bgColor Yellow $ [a]
    CRed a -> style Bold . bgColor Red $ [a]