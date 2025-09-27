module Koch.KochModule where

import qualified SDL
import SDL (($=))
import Linear.V2(V2(..))
import Linear.V4(V4(..))
import Control.Monad.Trans.State
import Core.CoreModule as CMM(DPoint(..), Size, Angle, AppM, myRight, myLeft, myRotate, step, drawLine_true, move)

-- модуль фрактал "Коха"

myFunctionTestKoch :: Int -> Int
myFunctionTestKoch x = x * 4

forward ::  CMM.Size -> SDL.Renderer -> CMM.AppM ()
forward vectorLength renderer = go vectorLength 5 renderer -- 4 итерации
  where
    go :: CMM.Size -> Int -> SDL.Renderer -> StateT CMM.DPoint IO ()
    go vectorLength n renderer = do
       if (n == 0) then
         step vectorLength  renderer
       else
         go (vectorLength / 3)  (n - 1)  renderer
         >> myLeft 90
         >> go (vectorLength / 3)  (n - 1)  renderer
         >> myRight 90
         >> go (vectorLength / 3)  (n - 1)  renderer
         >> myRight 90
         >> go (vectorLength / 3)  (n - 1)  renderer
         >> myLeft 90
         >> go (vectorLength / 3)  (n - 1)  renderer

-- Функция для однократной отрисовки сцены
drawScene :: SDL.Renderer -> CMM.DPoint -> IO ()
drawScene renderer initialPoint = do
    SDL.rendererDrawColor renderer $= V4 255 255 255 255 
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
  
    _ <- execStateT (do
       forward 400.0 renderer
       myRight 90         
       forward 400.0 renderer
       myRight 90
       forward 400.0 renderer
       myRight 90
       forward 400.0 renderer
       myRight 90) initialPoint
    SDL.present renderer
