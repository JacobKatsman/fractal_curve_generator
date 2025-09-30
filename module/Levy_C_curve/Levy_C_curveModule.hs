module Levy_C_curve.Levy_C_curveModule where

import qualified SDL
import SDL (($=))
import Linear.V2(V2(..))
import Linear.V4(V4(..))
import Control.Monad.Trans.State
import Core.CoreModule as CMM(DPoint(..), Size, Angle, AppM, myRight, myLeft, myRotate, step, drawLine_true,  move)

-- Variables: 	F
-- Constants: 	+ −
-- Start: 	F
-- Rules: 	F → +F−−F+

-- where "F" means "draw forward", "+" means "turn clockwise 45°", and "−" means "turn anticlockwise 45°". 

-- module ("Lévy C curve")
-- https://en.wikipedia.org/wiki/L%C3%A9vy_C_curve

forward ::  CMM.Size -> SDL.Renderer -> CMM.AppM ()
forward vectorLength renderer = go vectorLength 20 renderer
  where
    go :: CMM.Size -> Int -> SDL.Renderer -> StateT CMM.DPoint IO ()
    go vectorLength n renderer = do
       if (n == 0) then
         step vectorLength  renderer
       else

         myRight 45
         >> go (vectorLength * (1 / sqrt(2)))  (n - 1)  renderer
         >> myLeft 45
         >> myLeft 45
         >> go (vectorLength * (1 / sqrt(2)))  (n - 1)  renderer
         >> myRight 45 

-- Function for drawing a scene at once
drawScene :: SDL.Renderer -> CMM.DPoint -> IO ()
drawScene renderer initialPoint = do
    SDL.rendererDrawColor renderer $= V4 255 255 255 255 
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
    let initialPoint = CMM.DPoint { x = 400.0, y = 800.0, c = 90.0 }
    _ <- execStateT (do
       forward 500.0 renderer
       ) initialPoint
    SDL.present renderer

