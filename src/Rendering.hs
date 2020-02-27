module Rendering where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           Game

background :: Color
background = black

window :: Display
window =
  InWindow "hPong" (screenWidth, screenHeight) (screenOffset, screenOffset)
--  FullScreen

-- | Convert a game state into a picture.
render :: PongGame -> Picture
render game = case state game of
  Running          -> pictures [ball, walls, player1Paddle, player2Paddle]
  GameOver Player1 -> pictures [walls, player1Paddle]
  GameOver Player2 -> pictures [walls, player2Paddle]
 where
  player1Paddle =
    mkPaddle rose ((fromIntegral screenWidth / 2) - paddleWidth / 2)
      $ player1 game
  player2Paddle =
    mkPaddle orange (-(fromIntegral screenWidth / 2) + paddleWidth / 2)
      $ player2 game
  ball =
    uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
  ballColor = dark red

  --  The bottom and top walls.
  wall :: Float -> Picture
  wall offset = translate 0 offset $ color wallColor $ rectangleSolid
    (fromIntegral screenWidth)
    wallWidth

  wallColor = greyN 0.5
  walls     = pictures
    [ wall $ fromIntegral screenHeight / 2
    , wall $ fromIntegral (-screenHeight) / 2
    ]

  --  Make a paddle of a given border and vertical offset.
  mkPaddle :: Color -> Float -> Float -> Picture
  mkPaddle col x y = pictures
    [ translate x y $ color col $ rectangleSolid paddleWidth paddleHeight
    , translate x y $ color paddleColor $ rectangleSolid
      (paddleWidth - paddleBorderWidth)
      (paddleHeight - paddleBorderWidth)
    ]

  paddleColor = light (light blue)

