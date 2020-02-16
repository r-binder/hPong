module Game where

import Data.Array

type Radius = Float
type Point = (Float, Float)

data Player = Player1 | Player2 deriving (Eq, Show)
data State = Running | GameOver Player deriving (Eq, Show)

-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc    :: Point  -- ^ Pong ball (x, y) location.
  , ballVel    :: Point  -- ^ Pong ball (x, y) velocity. 
  , player1    :: Float  -- ^ Right player paddle height. Zero is the middle of the screen. 
  , player1Vel :: Float  -- ^ Right player paddle velocity.
  , player2    :: Float  -- ^ Left player paddle height.
  , player2Vel :: Float  -- ^ Left player paddle velocity. Zero is the middle of the screen. 
  , state :: State
  } deriving Show

paddleVel :: Float
paddleVel = 200

paddleWidth, paddleHeight, paddleBorderWidth :: Float
paddleWidth = 26
paddleHeight = 86
paddleBorderWidth = 6

wallWidth :: Float
wallWidth = 10

ballRadius :: Float
ballRadius = 10

screenWidth, screenHeight, screenOffset :: Int
screenWidth = 640
screenHeight = 480
screenOffset = 100

fps :: Int
fps = 60

initialState :: PongGame
initialState = Game
  { ballLoc    = (0, 0)
  , ballVel    = (-180, 100)
  , player1    = 0
  , player1Vel = 0
  , player2    = 0
  , player2Vel = 0
  , state      = Running
  }
