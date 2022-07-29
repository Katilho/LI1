module Types where

import Data.List

data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState = PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) = coloredString "#" None
   show (  Empty ) = coloredString " " None
   show (  Food z ) = coloredString (show z )   Green
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Normal)  ) Yellow
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Mega)  ) Blue
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = coloredString (show ( PacState (i, c, x, y,z,l) o m Dying)  ) Red
   show ( PacPlayer (Ghost z) ) = coloredString (show z)  Purple


coloredString :: String -> Color -> String
coloredString x y = x   {-
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 
   -}


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )


printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = []
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

-- * Funções adicionadas

-- | Função que dá as coordenadas do jogador.
getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Função que dá o número de vidas do jogador.
getPlayerLives :: Player -> Int
getPlayerLives (Pacman (PacState (x,y,z,t,h,l) q c d )) = l



-- | Função que devolve a lista de coordenadas duma lista de players.
getPlayersCoords :: [Player] -> [Coords]
getPlayersCoords [] = []
getPlayersCoords (x:xs) = getPlayerCoords x : getPlayersCoords xs

-- | Função que junta os players que são dead ghosts.
deadGhosts :: [Player] -> [Player]
deadGhosts [] = []
deadGhosts (p@(Pacman (PacState (a,b,c,d,e,f) g h i)) : ps) = deadGhosts ps
deadGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)) : ps) = deadGhosts ps
deadGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Dead)) : ps) = p : deadGhosts ps

-- | Função que dá as coordenadas dos Dead Ghosts que estão num determinado State.
ghostsDCoords :: State -> [Coords]
ghostsDCoords (State m x l) = getPlayersCoords (deadGhosts x)

-- | Função que junta os players que são Alive Ghosts.
aliveGhosts :: [Player] -> [Player]
aliveGhosts [] = []
aliveGhosts ((Pacman (PacState (a,b,c,d,e,f) g h i)) : ps) = aliveGhosts ps
aliveGhosts (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)) : ps) = p : aliveGhosts ps
aliveGhosts ((Ghost (GhoState (a,b,c,d,e,f) Dead)) : ps) = aliveGhosts ps

-- | Função que dá as coordenadas dos ghosts que estão num determinado State.
ghostsAlCoords :: State -> [Coords]
ghostsAlCoords (State m x l) = getPlayersCoords (aliveGhosts x)

-- | Função que verifica se um jogador é um Dead Ghost.
isDeadGhost :: Player -> Bool
isDeadGhost (Ghost (GhoState (a,b,c,d,e,f) Dead)) = True
isDeadGhost (Ghost (GhoState (a,b,c,d,e,f) Alive)) = False
isDeadGhost (Pacman (PacState (a,b,c,d,e,f) g h i)) = False

-- | Função que verifica se um conjunto de jogadores tem um Dead Ghost.
areDeadGhost :: [Player] -> Bool
areDeadGhost [] = False
areDeadGhost ((Ghost (GhoState (a,b,c,d,e,f) Dead)):xs) = True
areDeadGhost (x:xs) = areDeadGhost xs 

-- | Devolve a coordenada do interior da casa dos fantasmas relativamente a um dado labirinto, usada para mandar os fantasmas comidos para lá.
midCoord :: Maze   -- ^ Labirinto.
         -> Coords -- ^ Coordenadas da localização do interior da casa dos fantasmas no respetivo labirinto.
midCoord m@(x:xs) | even lm = ((div lm 2)-1, div lx 2)
                  | otherwise = (div lm 2, div lx 2)
                                where lm = length m -- Altura do labirinto.
                                      lx = length x -- Comprimento do labirinto.

-- | Dada uma lista de Players, devolve a lista das coordenadas de todos os fantasmas.
ghostsCoords :: [Player] -> [Coords]
ghostsCoords l = getPlayersCoords (ghostPlayers l)

-- | Dada uma lista de Players devolve o Player que é Pacman.
thePacman :: [Player] -> Player
thePacman (x:xs) | isPac x = x
                 | otherwise = thePacman xs

-- | Se o Player recebido for Pacman, devolve True, caso contrário devolve False.
isPac :: Player -> Bool
isPac (Pacman (PacState (a,b,c,d,e,f) g h i)) = True
isPac x = False

-- | Se o Player recebido for Ghost, devolve True, caso contrário devolve False.
isGhost :: Player -> Bool
isGhost (Ghost (GhoState (a,b,c,d,e,f) g)) = True
isGhost x = False

-- | Função que de um conjunto de Players, devolve o Player com o ID pedido.
idPlayer :: [Player] -> Int -> Player
idPlayer (x:xs) n | getPlayerID x == n = x
                  | otherwise = idPlayer xs n

-- | Função que devolve a lista de Ghosts que estão numa lista de Players.
ghostPlayers :: [Player] -> [Player]
ghostPlayers [] = []
ghostPlayers (x:xs) | isGhost x = x : ghostPlayers xs
                    | otherwise = ghostPlayers xs

-- | Função que diz em que coordenadas o Pacman se encontra no labirinto.
pacCoords :: State -> Coords
pacCoords (State m (x:xs) l) | isPac x = getPlayerCoords x
                             | otherwise = pacCoords (State m xs l)

-- | Função que devolve a coordenada imediatamente abaixo da coordenada recebida.
coo1D :: Coords -> Coords
coo1D (x,y) = (x+1,y)

-- | Dado uma Piece e um State, vai utilizar a função auxiliar, tendo como acumulador inicial (0,0), para determinar a lista das coordenadas da Piece dada.
getPieceCoords :: Piece -> State -> [Coords]
getPieceCoords p (State m x l) = auxGetPcCoords p m (0,0)


-- | Função auxiliar da função anterior que dado uma Piece, um Maze e Coords (acumulador, que será (0,0) para que determine corretamente as coordenadas do labirinto), devolvendo a lista de coordenadas onde a respetiva peça se encontra no labirinto.
auxGetPcCoords :: Piece -> Maze -> Coords -> [Coords]
auxGetPcCoords p [] (acx,acy) = []
auxGetPcCoords p ([]:ys) (acx,acy) = auxGetPcCoords p ys (acx+1,0)
auxGetPcCoords p ((x:xs):ys) (acx,acy) | p == x = (acx,acy) : auxGetPcCoords p (xs:ys) (acx,acy+1)
                                       | otherwise = auxGetPcCoords p (xs:ys) (acx,acy+1)
