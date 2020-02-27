module Game where

import           Data.Array

type Radius = Float
type Point = (Float, Float)

data Player = Player1 | Player2 deriving (Eq, Show)
data State = Running | GameOver Player deriving (Eq, Show)

-- | Data describing the state of the pong game. 
data PongGame = Game
  { ballLoc    :: Point  -- ^ Pong ball (x, y) location.
  , ballDir    :: Float  -- ^ Pong ball direction / angle.
  , ballVel    :: Float  -- ^ Pong ball velocity. 
  , player1    :: Float  -- ^ Right player paddle height. Zero is the middle of the screen. 
  , player1Vel :: Float  -- ^ Right player paddle velocity.
  , player2    :: Float  -- ^ Left player paddle height.
  , player2Vel :: Float  -- ^ Left player paddle velocity. Zero is the middle of the screen. 
  , state :: State
  } deriving Show

paddleVel :: Float
paddleVel = 500

paddleWidth, paddleHeight, paddleBorderWidth :: Float
paddleWidth = 26
paddleHeight = 86
paddleBorderWidth = 6

wallWidth :: Float
wallWidth = 10

ballRadius :: Float
ballRadius = 10

screenWidth, screenHeight, screenOffset :: Int
screenWidth = 1920
screenHeight = 1080
screenOffset = 100

fps :: Int
fps = 165

initialState :: PongGame
initialState = Game { ballLoc    = (0, 0)
                    , ballDir    = pi / 4
                    , ballVel    = 500
                    , player1    = 0
                    , player1Vel = 0
                    , player2    = 0
                    , player2Vel = 0
                    , state      = Running
                    }
