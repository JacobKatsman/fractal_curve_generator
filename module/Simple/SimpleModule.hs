module Simple.SimpleModule where

import qualified SDL
import SDL (($=))
import Linear.V2(V2(..))
import Linear.V4(V4(..))
import Control.Monad.Trans.State
import Core.CoreModule as CMM(DPoint(..), Size, Angle, AppM, myRight, myLeft, myRotate, step, drawLine_true, move)

--module ("simple five-pointed star")

-- Function for drawing a scene at once
drawScene :: SDL.Renderer -> CMM.DPoint -> IO ()
drawScene renderer initialPoint = do
    SDL.rendererDrawColor renderer $= V4 255 255 255 255 
    SDL.clear renderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 255
  
    _ <- execStateT (do
        step 400.0 renderer
        myRight 144
        step 400.0 renderer
        myRight 144
        step 400.0 renderer
        myRight 144
        step 400.0 renderer
        myRight 144
        step 400.0 renderer
        myRight 144
        step 400.0 renderer
        myRight 144
       ) initialPoint
         
    SDL.present renderer
