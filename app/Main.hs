
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified SDL
import SDL (($=))
import Linear.V2 ()
import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class()
import Data.Int() 
import Data.Text()
import Control.Monad.IO.Class(liftIO)
import Data.Text.IO
import Data.Text.IO as TIO
import System.IO as SIO
import Data.Time()
import Data.Time.Clock(getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Core.CoreModule as CMM(DPoint(..), Size, Angle, AppM, myRight, myLeft, myRotate, step, drawLine_true, move)
import Test.TestModule as TESTF(myFunctionTest) 
import Koch.KochModule as KOCHF(drawScene)
import Koch2.Koch2Module as KOCHF2(drawScene)
import Simple.SimpleModule as STAR(drawScene)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "SDL2 start Example" SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1200 1200 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  let initialPoint = CMM.DPoint { x = 400.0, y = 800.0, c = 90.0 }
  -- There are do call the calculation of our fractal curve
  -- KOCHF2.drawScene renderer initialPoint
  STAR.drawScene renderer initialPoint

  let loop = do
        events <- SDL.pollEvents
        let quit = any (== True) $ map eventIsQuit events
        SDL.present renderer
        unless quit loop
  loop
  
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

eventIsQuit :: SDL.Event -> Bool
eventIsQuit event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> True
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
      SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False



