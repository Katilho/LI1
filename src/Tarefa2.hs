module Tarefa2 where

import Types
import FileUtils
import Tarefa1 -- Para poder usar o generateMaze nos testes.


-- * Funções de movimento do Pacman

-- | Faz o Pacman se mover, consoante a orientação dada e a orientação do Pacman.
movePlayer :: Player      -- ^ Um determinado /Player/.
           -> Orientation -- ^ A orientação dada.
           -> Player      -- ^ /Player/ com as devidas alterações na sua posição ou orientação.
movePlayer p Null = p
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo Dying)) d = Pacman (PacState (id,(x,y),v,or,pts,l) mg mo Dying)
movePlayer (Pacman (PacState (id,(x,y),v,L,pts,l) mg mo st)) L = Pacman (PacState (id,(x,y-1),v,L,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,R,pts,l) mg mo st)) R = Pacman (PacState (id,(x,y+1),v,R,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,U,pts,l) mg mo st)) U = Pacman (PacState (id,(x-1,y),v,U,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,D,pts,l) mg mo st)) D = Pacman (PacState (id,(x+1,y),v,D,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) L = Pacman (PacState (id,(x,y),v,L,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) R = Pacman (PacState (id,(x,y),v,R,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) U = Pacman (PacState (id,(x,y),v,U,pts,l) mg mo st)
movePlayer (Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) D = Pacman (PacState (id,(x,y),v,D,pts,l) mg mo st)
movePlayer (Ghost (GhoState (id,(x,y),v,L,pts,l) st)) L = Ghost (GhoState (id,(x,y-1),v,L,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,R,pts,l) st)) R = Ghost (GhoState (id,(x,y+1),v,R,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,U,pts,l) st)) U = Ghost (GhoState (id,(x-1,y),v,U,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,D,pts,l) st)) D = Ghost (GhoState (id,(x+1,y),v,D,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) L = Ghost (GhoState (id,(x,y),v,L,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) R = Ghost (GhoState (id,(x,y),v,R,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) U = Ghost (GhoState (id,(x,y),v,U,pts,l) st)
movePlayer (Ghost (GhoState (id,(x,y),v,or,pts,l) st)) D = Ghost (GhoState (id,(x,y),v,D,pts,l) st)



-- | Faz o Pacman passar pelo túnel, se estiver para entrar nele, caso contrário faz o seu movimento normal básico, através da função anterior.
moveInTunnel :: Maze        -- ^ Labirinto onde se encontra o /Player/.
             -> Player      -- ^ O /Player/ selecionado.
             -> Orientation -- ^ Orientação ordenada.
             -> Player      -- ^ O /Player/ selecionado com a respetiva mudança na sua posição ou orientação.
moveInTunnel (m:ms) p@(Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)) dir
                            | y == 0 && dir == L && or == L = Pacman (PacState (id,(x,lgt),v,L,pts,l) mg mo st)
                            | y == lgt && dir == R && or == R = Pacman (PacState (id,(x,0),v,R,pts,l) mg mo st)
                            | otherwise = movePlayer p dir
                                        where lgt = (length m) - 1
moveInTunnel (m:ms) p@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)) dir
                            | y == 0 && dir == L && or == L = Ghost (GhoState (id,(x,lgt),v,L,pts,l) st)
                            | y == lgt && dir == R && or == R = Ghost (GhoState (id,(x,0),v,R,pts,l) st)
                            | otherwise = movePlayer p dir
                                        where lgt = (length m) - 1


-- | Função auxiliar que devolve o /Player/ ao qual é destinado o /Play/, ou seja, identifica o jogador com o ID da /Play/.
thePlayer :: Play     -- ^ Ordem /Play/ dirigida a um determinado /Player/ com uma certa ID. 
          -> [Player] -- ^ Lista de /Player/.
          -> Player   -- ^ /Player/ cuja /Play/ era dirigida, ou seja, o que tem o mesmo ID da /Play/.
thePlayer pl@(Move id or) (x:xs) | id == getPlayerID x = x
                                 | otherwise = thePlayer pl xs


-- | Função auxiliar que dá a peça que se encontra nas coordenadas do labirinto pedidas.
pieceInCoords :: Coords -- ^ Coordenadas do labirinto.
              -> Maze   -- ^ Respetivo labirinto. 
              -> Piece  -- ^ Peça que se encontra nas coordenadas do respetivo labirinto.
pieceInCoords (x,y) m = head $ drop y $ head $ drop x m


-- | Verifica se a posição para onde o Pacman vai, não tem uma peça /Wall/.
testWall :: Play  -- ^ Jogada. 
         -> State -- ^ Estado.
         -> Bool  -- ^ No caso de ter uma /Wall/ devolve /False/, caso contrário devolve /True/.
testWall (Move id Null) (State m x l) = True
testWall pl@(Move id or) (State m x l) | pieceInCoords (getPlayerCoords (moveInTunnel m (thePlayer pl x) or)) m == Wall = False
                                       | otherwise = True


-- | Dá uma lista de players com a alteração /Play/ destinada ao /Player/ com o respetivo ID.
moveID :: Play  -- ^ Jogada.
       -> State -- ^ Estado.
       -> State -- ^ Devolve um /State/ em que é feita a alteração no estado do respetivo /Player/ pretendida com o /Play/.
moveID (Move id or) (State m (x:xs) l) = (State m (map (\x -> if id==getPlayerID x then moveInTunnel m x or else x) (x:xs)) l)


-- | Função final que se aplicam todas as leis de movimento do Pacman.
movePac :: Play  -- ^ Jogada.
        -> State -- ^ Estado.
        -> State -- ^ Estado em que o /Player/ tem o seu estado alterado consoante a jogada, e que caso haja uma parede no caminho, ele não vá para ela.
movePac pl@(Move id or) s@(State m x l) | testWall pl s = moveID pl s
                                        | otherwise = State m x l


-- * Funções de atualização do score do Pacman

-- | Verifica se tem uma comida pequena nas coordenadas do Player com o ID do Play.
nextPieceSF :: Play  -- ^ Jogada.
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir uma comida pequena, caso contrário devolve False.
nextPieceSF (Move id or) (State m x l) | pieceInCoords (getPlayerCoords ((thePlayer (Move id or) x))) m == Food Little = True
                                       | otherwise = False

-- | Verifica se tem uma comida grande nas coordenadas do Player com o ID do Play.
nextPieceBF :: Play  -- ^ Jogada. 
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir uma comida grande, caso contrário devolve False.
nextPieceBF (Move id or) (State m x l) | pieceInCoords (getPlayerCoords ((thePlayer (Move id or) x))) m == Food Big = True
                                       | otherwise = False 

-- | Verifica se tem um fantasma morto nas coordenadas do Player com o ID do Play, através das coordenadas do Player e das coordenadas dos Ghosts Dead, verificando se a coordenada do Player coincide com a coordenada de algum Ghost, com a função "elem" pré-definida do Prelude.
nextPieceGh :: Play  -- ^ Jogada. 
            -> State -- ^ Estado.
            -> Bool  -- ^ Devolve True se nas coordenadas do Player existir um fantasma morto, caso contrário devolve False.
nextPieceGh (Move id or) (State m x l) | isPac (thePlayer (Move id or) x) && getPlayerID (thePlayer (Move id or) x) == id = elem (getPlayerCoords (thePlayer (Move id or) x)) (ghostsDCoords (State m x l))
                                       | otherwise = False


-- | Aumenta o score do Player com o ID do Play em 1 ponto.
score1ID :: Play     -- ^ Jogada com um determinado ID.
         -> [Player] -- ^ Conjunto de jogadores.
         -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +1 ponto.
score1ID pl [] = []
score1ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score1ID pl xs
score1ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs) 
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+1,l) mg mo st)) : xs
                                                         | otherwise = h : score1ID (Move id or) xs


-- | Aumenta o score do Player com o ID do Play em 5 pontos.
score5ID :: Play     -- ^ Jogada com um determinado ID.
         -> [Player] -- ^ Conjunto de jogadores.
         -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +5 pontos.
score5ID pl [] = []
score5ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score5ID pl xs
score5ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs)                                           
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+5,l) mg mo st)) : xs
                                                         | otherwise = h : score5ID (Move id or) xs                                                        


-- | Aumenta o score do Player com o ID do Play em 10 pontos.
score10ID :: Play     -- ^ Jogada com um determinado ID.
          -> [Player] -- ^ Conjunto de jogadores.
          -> [Player] -- ^ Conjunto de jogadores em que o jogador com o ID da /Play/ tem +10 pontos.
score10ID pl [] = []
score10ID pl (a@(Ghost (GhoState (n,(x,y),v,d,pts,l) st)):xs) = a : score10ID pl xs
score10ID (Move id or) (h@(Pacman (PacState (n,(x,y),v,d,pts,l) mg mo st)):xs)
                                                         | id == n = (Pacman (PacState (n,(x,y),v,d,pts+10,l) mg mo st)) : xs
                                                         | otherwise = h : score10ID (Move id or) xs


-- | Atualiza os pontos do State após uma jogada, verificando o que ele come para de seguida atribuir a respetiva pontuação a esse Pacman através das funções auxiliares anteriores.
makeScore :: Play  -- ^ Jogada.
          -> State -- ^ Estado.
          -> State -- ^ Estado atualizado, relativamente ao Score, tendo em conta o que o Pacman comeu.
makeScore pl@(Move id or) s@(State m x l) | nextPieceSF pl s && nextPieceGh pl s = State m (score1ID pl (score10ID pl x)) l
                                          | nextPieceBF pl s && nextPieceGh pl s = State m (score5ID pl (score10ID pl x)) l
                                          | nextPieceSF pl s = State m (score1ID pl x) l
                                          | nextPieceBF pl s = State m (score5ID pl x) l
                                          | nextPieceGh pl s = State m (score10ID pl x) l
                                          | otherwise = s



-- * Função de remoção de peça do labirinto

-- | Retira a comida do labirinto, após ser comida pelo Pacman através da função no ficheiro Types, que dado uma coordena, uma peça, e um labirinto, ela substitui a peça na coordenada pela peça pedida, nesse determinado labirinto. Para remover a peça comida damos a coordenada do Player, a peça Empty e o labirinto do State, para assim remover a peça.
clearFood :: Play  -- ^ Jogada.
          -> State -- ^ Estado.
          -> State -- ^ Estado atualizado relativamente à remoção de peças do labirinto, comidas pelo Pacman.
clearFood pl@(Move id or) s@(State m x l) 
                        | (nextPieceSF pl s || nextPieceBF pl s) && isPac (idPlayer x id) = State (replaceElemInMaze (getPlayerCoords (thePlayer pl x)) Empty m) x l
                        | otherwise = s



-- * Funções que controlam o nº de vidas do Pacman, bem como a sua atualização para o estado Dying.

-- | Verifica se tem um fantasma vivo nas coordenadas do Player com o ID do Play, através das coordenadas do Player e das coordenadas dos Ghosts Alive, verificando se a coordenada do Player coincide com a coordenada de algum Ghost com a função pré-definida "elem" do Prelude.
isAliveGhost :: Play  -- ^ Jogada.
             -> State -- ^ Estado.
             -> Bool  -- ^ Devolve True se algum fantasma vivo, está nas coordenadas do Pacman, caso contrário devolve False.
isAliveGhost (Move id or) (State m x l) = elem (getPlayerCoords (thePlayer (Move id or) x)) (ghostsAlCoords (State m x l))


-- | Função auxiliar que tira uma vida a um Player ou que passa o Pacman para Dying se não tiver vidas restantes. 
takeJgdrLife :: Play     -- ^ Jogada.
             -> [Player] -- ^ Conjunto de jogadores.
             -> [Player] -- ^ Conjunto de jogadores, em que é retirado uma vida ou passa a Dying se não tiver mais nenhuma, ao /Player/ com a mesma ID da /Play/.
takeJgdrLife pl [] = []
takeJgdrLife pl (a@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)):xs) = a : takeJgdrLife pl xs            
takeJgdrLife pl@(Move id or) (p@(Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo st)):xs)
                                                                         | id == n0 && getPlayerLives p > 0 = (Pacman (PacState (n0,(x,y),v,d,pts,l-1) mg mo st):xs)
                                                                         | id == n0 && getPlayerLives p == 0 = (Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo Dying):xs)
                                                                         | otherwise = p : takeJgdrLife pl xs


-- | Retira uma vida ao Pacman ou passa o Pacman a Dying se não tiver vidas restantes, quando o Pacman entra em contacto com um Ghost Alive.
takeLife :: Play  -- ^ Jogada.
         -> State -- ^ Estado.
         -> State -- ^ Estado em que se no seu [/Player/] houver algum Ghost vivo nas coordenadas do Pacman, então é retirado uma vida ou passa a Dying se não tiver vidas restantes.
takeLife pl@(Move id or) s@(State m x lv) | isAliveGhost pl s = State m (takeJgdrLife pl x) lv
                                          | otherwise = s



-- * Funções de transformação de estado dos Players

-- | Dado uma Play e um conjunto de players, torna o Pacman com o ID da Play em Mega.
turnToMega :: Play     -- ^ Jogada com um determinado ID.
           -> [Player] -- ^ Conjunto de jogadores.
           -> [Player] -- ^ Conjunto de jogadores em que o /Player/ com o ID da /Play/, passa a Mega.
turnToMega pl [] = []
turnToMega pl (a@(Ghost (GhoState (id,(x,y),v,or,pts,l) st)):xs) = a : turnToMega pl xs
turnToMega pl@(Move id or) (p@(Pacman (PacState (n0,(x,y),v,d,pts,l) mg mo st)):xs)
                                                  | id == n0 = (Pacman (PacState (n0,(x,y),v,d,pts,l) 10 mo Mega):xs)
                                                  | otherwise = p : turnToMega pl xs


-- | Função auxiliar para obter a orientação oposta, necessária para a Tarefa 5.
oposta :: Orientation -> Orientation
oposta Null = L
oposta L = R
oposta R = L
oposta D = U
oposta U = D


-- | Função auxiliar que transforma um conjunto de fantasmas vivos em fantasmas mortos e dando a orientação oposta à que tinham (a pedido da Tarefa 5), não fazendo alterações aos Players que não são fantasmas mortos.
transGhDead :: [Player] -- ^ Conjunto de jogadores.
            -> [Player] -- ^ Conjunto de jogadores em que os fantasmas que estavam vivos passaram a mortos.
transGhDead [] = []
transGhDead ((Ghost (GhoState (id,(x,y),v,or,pts,l) Alive)):xs) = Ghost (GhoState (id,(x,y),(v/2),(oposta or),pts,l) Dead) : transGhDead xs
transGhDead (x:xs) = x : transGhDead xs


-- | Transforma o Pacman em Mega (caso coma Big Food), consequentemente transformando todos os fantasmas vivos em mortos.
makeMega :: Play  -- ^ Jogada.
         -> State -- ^ Estado.
         -> State -- ^ Estado em que se o Pacman entrou em contacto com uma /Big Food/, então transforma-se em Mega e transforma todos os fantasmas vivos do [/Player/] em fantasmas mortos. Caso contrário, não há alterações relativamente ao PacMode do Pacman nem ao estado dos fantasmas.
makeMega pl@(Move id or) s@(State m x l) | nextPieceBF pl s && isPac (idPlayer x id) = State m (transGhDead (turnToMega pl x)) l
                                         | otherwise = s 



-- * Funções de teleporte dos fantasmas para a sua casa

-- | Função auxiliar que manda um Ghost comido pelo Pacman para a casa dos fantasmas, retornando o seu estado para Alive e a sua velocidade para o normal (multiplicando por 2).
resetSpawn :: Coords   -- ^ Coordenadas pedidas.
           -> State    -- ^ Estado com um determinado labirinto.
           -> [Player] -- ^ Conjunto de jogadores em que se um /Ghost Dead/ estiver nas coordenadas pedidas, ele é mandado para as coordenadas obtidas através da função auxiliar anterior (que o mandam para a casa dos fantasmas) e o seu estado volta a ser Normal. 
resetSpawn c (State m [] l) = [] 
resetSpawn c (State m (h@(Pacman (PacState (id,(x,y),v,or,pts,l) mg mo st)):xs) lv) = h : resetSpawn c (State m xs lv)
resetSpawn c (State m (h@(Ghost (GhoState (id,(x,y),v,or,pts,l) Alive)):xs) lv) = h : resetSpawn c (State m xs lv)
resetSpawn c (State m (h@(Ghost (GhoState (id,(x,y),v,or,pts,l) Dead)):xs) lv)
                                          | c == (x,y) = (Ghost (GhoState (id,midCoord m,v*2,or,pts,l) Alive)):resetSpawn c (State m xs lv)
                                          | otherwise = h : resetSpawn c (State m xs lv)


-- | Dado um /Play/ e um /State/, dá um /State/ com a alteração nas coordenadas e no estado do /Ghost/ comido caso haja algum Pacman que tenha comido um /Ghost Dead/, caso contrário não há alterações nas coordenadas nem no estado de nenhum /Ghost/.
goHome :: Play  -- ^ Jogada com um determinado ID.
       -> State -- ^ Estado.
       -> State -- ^ Estado em que se o Pacman com o determinado ID, se encontra nas mesmas coordenadas de um /Ghost Dead/, então esse /Ghost/ é enviado para a casa dos fantasmas e o seu estado e velocidade voltam ao normal.
goHome pl@(Move id or) s@(State m x l) | nextPieceGh pl s = State m (resetSpawn (getPlayerCoords(thePlayer pl x)) s) l
                                       | otherwise = s


-- * Função final

-- | Função final com todas as ações que o Pacman tem de realizar no decurso de uma jogada.
play :: Play  -- ^ Jogada.
     -> State -- ^ Estado. 
     -> State -- ^ Estado final em que dependendo da jogada realizada, o estado é atualizado, tendo em conta todas as ações a realizar.
play p@(Move id or) s@(State m x l) = checkMega $ checkTimeMg $ moveMouth $ goHome p $ takeLife p $ makeMega p $ makeScore p $ movePac p $ clearFood p s



-- * Funções complementares para a 2ª fase

-- | Fecha a boca aos Pacmans com boca aberta e vice-versa numa lista de jogadores. (talvez meter um matching para caso o pacman esteja dying)
ismouth :: [Player] -> [Player]
ismouth [] = []
ismouth ((Pacman (PacState (x,y,z,t,h,l) b Open d)):xs) = (Pacman (PacState (x,y,z,t,h,l) b Closed d)) : ismouth xs
ismouth ((Pacman (PacState (x,y,z,t,h,l) b Closed d)):xs) = (Pacman (PacState (x,y,z,t,h,l) b Open d)) : ismouth xs
ismouth (x:xs) = x : ismouth xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior de fechar e abrir a boca aos Pacmans, consoante o necessário. 
moveMouth :: State -> State
moveMouth (State m x l) = State m (ismouth x) l



-- | Verifica se há algum Pacman no modo Mega.
areMega :: [Player] -> Bool
areMega [] = False
areMega ((Pacman (PacState (x,y,z,t,h,l) b c Mega)):xs) = True
areMega (x:xs) = areMega xs

-- | Transforma todos os fantasmas mortos em fantasmas vivos.
turnToAlive :: [Player] -> [Player]
turnToAlive [] = []
turnToAlive ((Ghost (GhoState (x,y,z,t,h,l) Dead)):xs) = (Ghost (GhoState (x,y,z,t,h,l) Alive)) : turnToAlive xs
turnToAlive (x:xs) = x : turnToAlive xs

-- | Transforma os fantasmas mortos em vivos se não houver nenhum Pacman no modo Mega.
checkMegaPl :: [Player] -> [Player]
checkMegaPl [] = []
checkMegaPl x | areMega x = x
              | otherwise = turnToAlive x

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que transforma os fantasmas mortos em fantasmas vivos se não houver nenhum Pacman em modo Mega.
checkMega :: State -> State
checkMega (State m x l) = State m (checkMegaPl x) l 



-- | Verifica se um Pacman tem MegaTime <= 0 estando em modo Mega, transformando-o para o estado Normal nesse caso. (TALVEZ METER AQUI O TIRAR O TEMPO DO MEGATIME POR PLAY)
checkTimeAux :: [Player] -> [Player]
checkTimeAux [] = []
checkTimeAux (p@(Pacman (PacState (x,y,z,t,h,l) b c Mega)):xs) | b <= 0 = (Pacman (PacState (x,y,z,t,h,l) 0 c Normal)) : checkTimeAux xs
                                                               | otherwise = p : checkTimeAux xs
checkTimeAux (x:xs) = x : checkTimeAux xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que transforma um Pacman para o estado Normal se um Pacman tem MegaTime <= 0 estando em modo Mega.
checkTimeMg :: State -> State
checkTimeMg (State m x l) = State m (checkTimeAux x) l



-- * Funções-teste.
-- | Exemplo de labirinto.
m1 :: Maze
m1 = generateMaze 15 10 54

-- | Casos de Plays e States para a função play.
testCasesPlay :: [(Play,State)]
testCasesPlay = [((Move 0 R),State m1 [(Pacman (PacState (0,(1,9),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,3),2,L,0,2) 0 Open Normal))] 1),((Move 0 L), State m1 [(Pacman (PacState (0,(1,9),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,7),2,L,0,2) 0 Closed Normal))] 2), ((Move 0 R), State m1 [(Pacman (PacState (0,(2,8),2,R,0,2) 10 Open Mega)),(Ghost (GhoState (1,(2,9),2,L,0,2) Dead))] 1), ((Move 0 R),State m1 [(Pacman (PacState (0,(5,14),2,R,0,2) 0 Open Normal)),(Pacman (PacState (1,(2,3),2,L,0,2) 0 Open Normal))] 1)]

-- | Testa os casos para a função play.
testItT2 :: [(Play,State)] -> [State]
testItT2 [] = []
testItT2 ((a,b):xs) = play a b : testItT2 xs


-- | Testa se algum /Player/ de um /State/, se encontra indevidamente nas coordenadas de uma /Wall/, dando /True/ caso haja.
isOnWall :: State -> Bool
isOnWall (State m [] l) = False
isOnWall (State m (p:ps) l) | pieceInCoords (getPlayerCoords p) m == Wall = True
                            | otherwise = isOnWall (State m ps l)
