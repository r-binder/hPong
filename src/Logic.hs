module Logic where

import Game
import Graphics.Gloss.Interface.Pure.Game hiding (Point)

-- | Update the ball position using its current velocity.
moveBall
  :: Float    -- ^ The number of seconds since last update
  -> PongGame -- ^ The initial game state
  -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
 where
  -- Old locations and velocities.
  (x , y ) = ballLoc game
  (vx, vy) = ballVel game

  -- New locations.
  x'       = x + vx * seconds
  y'       = y + vy * seconds


bounce :: PongGame -> PongGame
bounce game = game { ballVel = (vx', vy') }
 where
  -- Old locations and velocities.
  (vx, vy) = ballVel game
  (bx, by) = ballLoc game
  p1       = player1 game
  p2       = player2 game
  ballPos  = (ballLoc game)

  -- New velocities.
  vy'      = if ((vy < 0) && topCollision) || ((vy > 0) && bottomCollision)
    then -vy
    else vy
  topCollision = by - ballRadius <= -fromIntegral screenHeight / 2
  bottomCollision = by + ballRadius >= fromIntegral screenHeight / 2

  vx' = if ((vx > 0) && player1Collision) || ((vx < 0) && player2Collision)
    then -vx
    else vx
  player2Collision =
    (bx - ballRadius <= (-fromIntegral screenWidth / 2) + paddleWidth)
      && checkPlayerHeight p2
  player1Collision =
    (bx + ballRadius >= (fromIntegral screenWidth / 2) - paddleWidth)
      && checkPlayerHeight p1
  checkPlayerHeight p =
    ((by + ballRadius) <= p + paddleHeight / 2)
      && ((by + ballRadius) >= p - paddleHeight / 2)

-- | Update the player positions using its current velocity.
movePlayers :: Float -> PongGame -> PongGame
movePlayers seconds game = game { player1 = p1', player2 = p2' }
 where
  p1          = player1 game
  p2          = player2 game
  v1          = player1Vel game
  v2          = player2Vel game
  bottomBound = -(fromIntegral screenHeight / 2) + wallWidth
  topBound    = (fromIntegral screenHeight / 2) - wallWidth
  paddleSize  = paddleHeight / 2
  p1' = if (v1 > 0 && checkTopBound p1) || (v1 < 0 && checkBottomBound p1)
    then p1 + v1 * seconds
    else p1
  p2' = if (v2 > 0 && checkTopBound p2) || (v2 < 0 && checkBottomBound p2)
    then p2 + v2 * seconds
    else p2
  checkTopBound p = (p + paddleSize) <= topBound
  checkBottomBound p = (p - paddleSize) >= bottomBound

checkGameOver :: PongGame -> PongGame
checkGameOver game
  | bx - ballRadius <= -bound = game { state = GameOver Player1 }
  | bx + ballRadius >= bound  = game { state = GameOver Player2 }
  | otherwise                 = game
 where
  (bx, _) = ballLoc game
  bound   = (fromIntegral screenWidth / 2)

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> PongGame -> PongGame
update seconds game = if state game == Running
  then checkGameOver . bounce . movePlayers seconds . moveBall seconds $ game
  else game

-- | Respond to key events.
handleKeys :: Event -> PongGame -> PongGame

handleKeys (EventKey (Char 'n') _ _ _) game = initialState

handleKeys (EventKey (Char 'w') Down _ _) game =
  game { player2Vel = paddleVel }
handleKeys (EventKey (Char 'w') Up _ _) game = game { player2Vel = 0 }

handleKeys (EventKey (Char 's') Down _ _) game =
  game { player2Vel = -paddleVel }
handleKeys (EventKey (Char 's') Up _ _) game = game { player2Vel = 0 }

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
  game { player1Vel = paddleVel }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player1Vel = 0 }

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
  game { player1Vel = -paddleVel }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game =
  game { player1Vel = 0 }

handleKeys _ game = game
