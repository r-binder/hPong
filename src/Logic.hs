module Logic where

import           Game
import           Graphics.Gloss.Interface.Pure.Game
                                         hiding ( Point )

getDir :: Float -> Float -> Float
getDir dx dy | dx > 0 && dy < 0   = angle + (2 * pi)
             | dx <= 0 || dy <= 0 = angle + pi
             | otherwise          = angle
  where angle = atan $ dy / dx

-- | Update the ball position using its current velocity.
moveBall
  :: Float    -- ^ The number of seconds since last update
  -> PongGame -- ^ The initial game state
  -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
 where
  -- Old locations and velocities.
  (x, y) = ballLoc game
  angle  = ballDir game
  dx     = cos angle
  dy     = sin angle
  vel    = ballVel game

  -- New locations.
  x'     = x + (dx * vel * seconds)
  y'     = y + (dy * vel * seconds)


bounce :: PongGame -> PongGame
bounce game = game { ballDir = getDir dx' dy' }
 where
  -- Old locations and directions.
  angle    = ballDir game
  dx       = cos angle
  dy       = sin angle
  (bx, by) = ballLoc game
  p1       = player1 game
  p2       = player2 game
  ballPos  = (ballLoc game)

  -- New velocities.
  dy'      = if ((dy < 0) && topCollision) || ((dy > 0) && bottomCollision)
    then -dy
    else dy
  topCollision = by - ballRadius <= -fromIntegral screenHeight / 2
  bottomCollision = by + ballRadius >= fromIntegral screenHeight / 2

  dx' = if ((dx > 0) && player1Collision) || ((dx < 0) && player2Collision)
    then -dx
    else dx
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

handleKeys (EventKey (Char 'n') Down _ _) game = initialState

handleKeys (EventKey (Char 'w') Down _ _) game =
  game { player2Vel = paddleVel }
handleKeys (EventKey (Char 'w') Up _ _) game = game { player2Vel = vel' }
 where
  vel  = player2Vel game
  vel' = if vel > 0 then 0 else vel

handleKeys (EventKey (Char 's') Down _ _) game =
  game { player2Vel = -paddleVel }
handleKeys (EventKey (Char 's') Up _ _) game = game { player2Vel = vel' }
 where
  vel  = player2Vel game
  vel' = if vel < 0 then 0 else vel

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game =
  game { player1Vel = paddleVel }
handleKeys (EventKey (SpecialKey KeyUp) Up _ _) game = game { player1Vel = vel'
                                                            }
 where
  vel  = player1Vel game
  vel' = if vel > 0 then 0 else vel

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game =
  game { player1Vel = -paddleVel }
handleKeys (EventKey (SpecialKey KeyDown) Up _ _) game = game
  { player1Vel = vel'
  }
 where
  vel  = player1Vel game
  vel' = if vel < 0 then 0 else vel

handleKeys _ game = game
