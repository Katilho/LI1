module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Tarefa5
import Tarefa6


data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int
    ,    step   :: Int
    ,    before :: Integer
    ,    delta  :: Integer
    ,    delay  :: Integer
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )



-- | Configura a orientação de um /Player/ de acordo com a orientação dada.
setPlayerOrientation :: Player      -- ^ O /Player/ selecionado.
                     -> Orientation -- ^ A orientação desejada.
                     -> Player      -- ^ O /Player/ com a orientação desejada.
setPlayerOrientation (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) o = (Pacman (PacState (id,(x,y),v,o,pts,l) mg mo st))
setPlayerOrientation (Ghost (GhoState (id,(x,y),v,or,pts,l) gm)) o = (Ghost (GhoState (id,(x,y),v,o,pts,l) gm))


changeOrID :: Int -> Orientation -> [Player] -> [Player]
changeOrID _ _ [] = []
changeOrID n or (x:xs) | getPlayerID x == n = (setPlayerOrientation x or) : xs
                       | otherwise = x : changeOrID n or xs


changeOrientation :: Orientation -> Manager -> Manager
changeOrientation ori (Manager (State m x l) id b c d e) = (Manager (State m (changeOrID id ori x) l) id b c d e)


updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyUpArrow    m = changeOrientation U m 
updateControlledPlayer KeyDownArrow  m = changeOrientation D m
updateControlledPlayer KeyLeftArrow  m = changeOrientation L m
updateControlledPlayer KeyRightArrow m = changeOrientation R m
updateControlledPlayer k m = m


updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager state pid step before delta delay) = (Manager state pid step now (delta+(now-before)) delay)

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager state pid step before delta delay) = (Manager state pid step now 0 delay)

nextFrame :: Integer -> Manager -> Manager
nextFrame now man = let update = (resetTimer now man)
                    in update  { state = (passTime (step man) (state man)) , step = (step man) + 1}



loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorDefault  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager
