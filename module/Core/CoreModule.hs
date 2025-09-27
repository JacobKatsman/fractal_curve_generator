{-# LANGUAGE OverloadedStrings #-}
module Core.CoreModule where

import qualified SDL
import SDL (($=))
import Linear.V2(V2(..))
import Control.Monad (unless)
import Foreign.C.Types (CInt)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class ()
import Data.Int() 
import Data.Text()
import Control.Monad.IO.Class (liftIO)
import Data.Text.IO
import Data.Text.IO as TIO
import System.IO as SIO
import Data.Time ()
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

data DPoint = DPoint { x :: Double, y :: Double, c :: Double } deriving (Show, Eq)
type Size = Double  
type Angle = Double 
type AppM = StateT DPoint IO

-- Функция для логирования текста в файл
-- logMsg :: String -> AppM ()
-- logMsg msg = do
--     currentTime <- liftIO getCurrentTime
     --let formattedDateTime = formatTime defaultTimeLocale "%d.%m.%Y-%H:%M:%S" currentTime
--     liftIO $ SIO.appendFile ("log-" ++ "01" ++ ".txt") (msg ++ "\n")

-- перенос 
move :: Size ->  AppM()
move vectorLength = do
    current <- Control.Monad.Trans.State.get
    let a = vectorLength * sin ((c current) * (pi / 180))
    let b = vectorLength * cos ((c current) * (pi / 180))
    put $ DPoint { x = x current +  a, y = y current + b, c = c current}

-- поворот налево
myLeft :: Angle -> AppM()
myLeft  angle = do
        current <- Control.Monad.Trans.State.get
        let angleCurrent = angle * (-1)
        myRotate angleCurrent

-- поворот направо
myRight :: Angle -> AppM()
myRight  angle = do
        current <- Control.Monad.Trans.State.get
        let angleCurrent = angle 
        myRotate angleCurrent
        
-- поворот
myRotate :: Angle -> AppM()
myRotate  angle = do
        current <- Control.Monad.Trans.State.get
        let angleCurrent = (c current)  +  angle
        modify (\current -> current { c = angleCurrent })

-- шаг "вперед"
step :: Size -> SDL.Renderer -> AppM()
step vectorLength renderer = do

       current <- Control.Monad.Trans.State.get
       let x1 = fromIntegral (round (x current)) :: CInt 
       let y1 = fromIntegral (round (y current)) :: CInt
       let x1Double = x current 
       let y1Double = y current
       move vectorLength
       newPoint <- Control.Monad.Trans.State.get
       let x2 = fromIntegral (round (x newPoint)) :: CInt 
       let y2 = fromIntegral (round (y newPoint)) :: CInt
       let x2Double = x current 
       let y2Double = y current
       {--logMsg $ " x1 = " ++ show (x1Double)  ++ " y1 = "
                         ++ show (y1Double)  ++ " x2 = "
                         ++ show (x2Double)  ++ " y2 = "
                         ++ show (y2Double) ++ "; angle "
                         ++ show (c current) ++ "; size ="
                         ++ show (vectorLength) --}
                         
       liftIO $ drawLine_true renderer (SDL.P (V2 x1 y1)) (SDL.P (V2 x2 y2))

drawLine_true :: SDL.Renderer -> SDL.Point V2 CInt -> SDL.Point V2 CInt -> IO ()
drawLine_true renderer start end = SDL.drawLine renderer start end
