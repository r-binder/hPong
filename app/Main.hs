module Main where

import Graphics.Gloss

import Game
import Logic
import Rendering

main :: IO ()
main = play window background fps initialState render handleKeys update
