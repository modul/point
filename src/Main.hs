{-# LANGUAGE RecordWildCards #-}

module Main where

import Debug.Trace
import Graphics.Gloss

data Ball = Ball {
             velocity :: Float,       -- ^ initial velocity since change of direction
             height :: Float,         -- ^ initial height
             gravity :: Float,        -- ^ gravity 
             position :: Float,       -- ^ current position
             radius :: Float,         -- ^ size of the ball
             bounce :: Float          -- ^ coefficient of restitution 
            } deriving Show

moveBall :: Float -> Ball -> Ball
moveBall dt b@Ball{..} = trace (show (s, v')) $ b {velocity = v', position = s'}
    where s = position + velocity 
          s' = if v' == 0 then 0 else s
          v' = let v = abs velocity in 
                if velocity <= 0 && s <= 0 then
                 if v < 0.1 then 0 else bounce * v
                else velocity - gravity * dt

initialState b h r = Ball 0.0 h 9.81 h r b

drawBall b = Color red $ Translate 0 y $ circleSolid rad
    where y = position b
          rad = radius b

drawText size = Scale size size . Text

drawStats b = Translate 0 (-60) $ pictures [box, vel, pos]
    where box = rectangleWire 450 60
          pos = Translate (-190) (-10) $ drawText size $ "s: " ++ (show $ position b)
          vel = Translate    10  (-10) $ drawText size $ "v: " ++ (show $ velocity b)
          size = 0.20

render :: Float -> Ball -> Picture
render offset ball = Translate 0 offset $ pictures [drawStats ball, drawBall ball]

fps = 60

disp = InWindow title (width, height) position
    where height = 800
          width = 800
          position = (0, 0)
          title = "Der springende Punkt."

background = white

update _ = moveBall 

infiniteBounce = initialState 1.0
finiteBounce = initialState 0.85

main :: IO ()
main = simulate disp background fps (finiteBounce 500 30) (render (-200)) update

