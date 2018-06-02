{-# LANGUAGE RecordWildCards #-}

module Main where

import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data Ball = Ball {
             run :: Bool,            -- ^ run movement
             velocity :: Float,       -- ^ current velocity
             height :: Float,         -- ^ initial height
             gravity :: Float,        -- ^ gravity 
             position :: Float,       -- ^ current position
             radius :: Float,         -- ^ size of the ball
             bounce :: Float          -- ^ coefficient of restitution              
            } deriving Show

moveBall :: Float -> Ball -> Ball
moveBall dt b@Ball{..} = trace (show (v', s')) $ go
    where go = if run then b {velocity = v', position = s'} else b
          s = position + velocity 
          s' = if v' == 0 then 0 else s
          v' = let v = abs velocity in 
                if velocity <= 0 && s <= 0 then
                 if v < 0.1 then 0 else bounce * v
                else velocity - gravity * dt

initialState b h g r = Ball True (-g) h g h r b

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

update = moveBall

handle (EventKey (SpecialKey KeySpace) Down _ _) ball@Ball{..} = ball {run = (not run)}
handle (EventKey (SpecialKey KeyDown ) Down _ _) ball@Ball{..} = ball {velocity = (-2) * gravity }
handle (EventKey (SpecialKey KeyEnter) Down _ _) ball@Ball{..} = ball {position = height, velocity = (-gravity)}
handle _ b = b

infiniteBounce = initialState 1.0 300
finiteBounce = initialState 0.85 500

main :: IO ()
main = game

simulation = simulate disp background fps (finiteBounce 9.81 30) (render (-200)) (\viewPort -> update)
game = play disp background fps (infiniteBounce 9.81 30) (render (-200)) handle update
