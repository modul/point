{-# LANGUAGE RecordWildCards #-}

module Main where

import Debug.Trace
import Graphics.Gloss

data Direction = Up | Down deriving (Eq, Show)

data Ball = Ball {
             direction :: Direction,  -- ^ current direction
             velocity :: Float,       -- ^ initial velocity since change of direction
             height :: Float,         -- ^ initial height
             gravity :: Float,        -- ^ gravity 
             position :: Float,       -- ^ current position
             diameter :: Float       -- ^ size of the ball
            } deriving Show

moveBall :: Ball -> Float -> Ball
moveBall b@Ball{..} dt = trace (show (s, d', v')) $ b {direction = d', velocity = v', position = s}
    where s = position + velocity
          d' = if direction == Up && s >= height then Down 
                else if direction == Down && s <= 0 then Up
                 else direction
          v' = if direction == Up then 
                 velocity - gravity * dt
                else if s <= 0 then abs velocity else velocity - gravity * dt

initialState h d = Ball Down 0.0 h 9.81 h d

drawBall b = Color red $ Translate 0 y $ ThickCircle rad dia
    where y = position b
          dia = diameter b
          rad = 0.5 * dia

drawGround p = Line [(-100, p), (100, p)]

render :: Float -> Ball -> Picture
render offset ball = Translate 0 offset $ pictures [drawGround (-30) , drawBall ball]
        

fps = 60

disp = InWindow title (width, height) position
    where height = 800
          width = 800
          position = (0, 0)
          title = "Der springende Punkt."

background = white

update _ t b = moveBall b t

main :: IO ()
main = simulate disp background fps (initialState 300 30) (render (-200)) update

