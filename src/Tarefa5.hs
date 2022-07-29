{- |

= Introdução

Esta tarefa consistiu em implementar um comportamento para os fantasmas. Quer para fugir do Pacman, quer para ir atrás do Pacman. A tarefa inicialmente aparentou ser um pouco intimidadora, uma vez que teríamos de ter várias possibilidades e variáveis em consideração para determinar a melhor jogada possível.

= Objetivos

Inicialmente foi necessário fazer alterações na Tarefa 2 para admitir o movimento dos fantasmas o que provou ser um pouco trabalhoso, uma vez que implicou fazer várias alterações em várias funções da Tarefa 2 para assegurar o correto funcionamento do jogo. 

Para saber que jogada fazer, é necessário saber as coordenadas do fantasma (em questão) e do Pacman. Para isso elaboramos novas funções e usamos também algumas previamente feitas para conseguir extrair as coordenadas dos Players corretamente.
Após ter as funções de extração de coordenadas, era necessário funções que analisassem essas coordenadas e a sua contextualização no labirinto para que após a análise, devolvesse uma jogada a fazer.
Para tal, foram feitas funções seguindo as sugestões do enunciado do trabalho prático, as quais sugeriam que tentassemos determinar a direção a tomar para o fantasma se aproximar o máximo possível do Pacman.
Após os professores terem anunciado que tinhamos maior liberdade do que estava previsto no enunciado, decidimos melhorar um pouco mais a estratégia dos fantasmas adicionando mais algumas possibilidades, que não estavam a ser previstas pelo fantasma, tendo sido melhorada minimamente a eficácia dos fantasmas, relativamente à estratégia utilizada anteriormente.

Com a inteligência dos fantasmas quisemos que eles fossem desafiantes, mas também não queriamos que os fantasmas fossem extremamente eficazes a ir atrás do Pacman, algo que poderia ser frustrante para o jogador.

= Discussão e conclusão

Foi divertido ver nesta tarefa o resultado das linhas de código que escrevemos em ação e fugirmos dos fantasmas, brincando com o jogo para o testar, tal como se estivessemos a jogar o jogo original do Pacman.
Ficamos satisfeitos com o desempenho da inteligência artificial dos fantasmas, apesar de ter demonstrado ser uma tarefa trabalhosa e complicada.

-}

module Tarefa5 where 

import Types
import Tarefa2


-- | Função que diz em que coordenadas está o Ghost com o determinado ID.
ghostIDCoords :: State -> Int -> Coords
ghostIDCoords (State m x l) id = getPlayerCoords (idPlayer x id)


-- | Função que dá a coordenada para a determinada orientação que é recebida como argumento, relativamente à coordenada inicial pedida.
chCoord :: Coords -> Maze -> Orientation -> Coords
chCoord (x,y) (m:ms) L | (y-1) < 0 = (x,(length m)-1)
                       | otherwise = (x,y-1)
chCoord (x,y) (m:ms) R | (y+1) > ((length m)-1) = (x,0)
                       | otherwise = (x,y+1)
chCoord (x,y) (m:ms) U = (x-1,y)
chCoord (x,y) (m:ms) D = (x+1,y) 
chCoord (x,y) (m:ms) Null = (x,y)


-- | Função que testa se há uma parede no sitio para onde o Player iria tendo em conta a orientação recebida como argumento.
nxtWall :: Coords -> Maze -> Orientation -> Bool
nxtWall c m or | pieceInCoords (chCoord c m or) m == Wall = True
               | otherwise = False


-- | Dada uma coordenada e uma orientação, devolve o conjunto das coordenadas, que se encontram 5 unidades de distancia relativamente à orientação fornecida.
listCoords6 :: Coords -> Orientation -> [Coords]
listCoords6 (x,y) L    = [(x,y-1), (x,y-2), (x,y-3), (x,y-4), (x,y-5)]
listCoords6 (x,y) R    = [(x,y+1), (x,y+2), (x,y+3), (x,y+4), (x,y+5)]
listCoords6 (x,y) U    = [(x-1,y), (x-2,y), (x-3,y), (x-4,y), (x-5,y)]
listCoords6 (x,y) D    = [(x+1,y), (x+2,y), (x+3,y), (x+4,y), (x+5,y)]
listCoords6 (x,y) Null = [(x,y+1), (x,y+2), (x,y+3), (x,y+4), (x,y+5)]


-- | Recebe o conjunto de coordenadas, produzido pela função anterior e filtra as coordenadas que se encontram fora do labirinto e que incluam as paredes que cobrem o labirinto, e pára de construir a lista quando uma das coordenadas coincide com as do Pacman.
filterCoords :: [Coords] -- ^ Conjunto de coordenadas criadas pela função anterior. 
             -> Maze     -- ^ Labirinto onde se enquadram as coordenadas.
             -> Coords   -- ^ Coordenadas da posição do Pacman.
             -> [Coords] -- ^ Resultado final filtrado pelas restrições programadas.
filterCoords [] m _ = []
filterCoords (a@(x,y):xs) mz@(m:ms) cp | a == cp = []
                                       | x <= 0 || x >= ((length mz)-1) || y <= 0 || y >= ((length m)-1) = []
                                       | otherwise = a : filterCoords xs mz cp


-- | Verifica (com uma função da Tarefa2) cada coordenada da lista filtrada anteriormente e se houver alguma coordenada em que haja uma Wall no labirinto, devolve False.
checkRetaWall :: [Coords] -> Maze -> Bool
checkRetaWall [] m = True
checkRetaWall (a@(x,y):xs) m | pieceInCoords a m == Wall = False
                             | otherwise = checkRetaWall xs m 


-- | Função que utiliza todas as funções auxiliares anteriores, para verificar se o fantasma pode ir atrás do Pacman sem ir contra nenhuma parede.
checkForWalls :: Coords      -- ^ Coordenadas do Ghost. 
              -> Orientation -- ^ Orientação para a qual quer verificar se há paredes.
              -> Maze        -- ^ Labirinto onde se enquadram as coordenadas.
              -> Coords      -- ^ Coordenadas do Pacman.
              -> Bool        -- ^ Retornará True se o caminho estiver livre, caso contrário retornará False.
checkForWalls c or m cp = checkRetaWall (filterCoords (listCoords6 c or) m cp) m



-- | Função auxiliar que dado um State, um ID, as coordenadas do Pacman e do Ghost com o determinado ID, decide a jogada a fazer para o Ghost ir atrás do Pacman.
coordsChase :: State  -- ^ Estado do jogo.
            -> Int    -- ^ ID de um determinado fantasma.
            -> Coords -- ^ Coordenadas do Pacman.
            -> Coords -- ^ Coordenadas do fantasma com o determinado ID.
            -> Play   -- ^ Play a executar pelo fantasma com o respetivo ID com a intenção de ir atrás do Pacman.
coordsChase s@(State m x l) id cp@(x1,y1) c@(x2,y2) 
                                 | getPlayerCoords (idPlayer x id) == midCoord m || getPlayerCoords (idPlayer x id) == coo1D (midCoord m) = (Move id U)  -- para sair da casa dos fantasmas
                                 | nxtWall gcoords m L && nxtWall gcoords m U && nxtWall gcoords m D = (Move id R)
                                 | nxtWall gcoords m R && nxtWall gcoords m U && nxtWall gcoords m D = (Move id L)
                                 | nxtWall gcoords m L && nxtWall gcoords m R && nxtWall gcoords m D = (Move id U)
                                 | nxtWall gcoords m L && nxtWall gcoords m R && nxtWall gcoords m U = (Move id D)
                                 | nxtWall gcoords m orient && x1 == x2 && y1 > y2 = (Move id R)
                                 | nxtWall gcoords m orient && x1 == x2 && y1 < y2 = (Move id L)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 == y2 = (Move id U)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 == y2 = (Move id D)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 < y2 && (x2-x1) >= (y2-y1) = (Move id U)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 < y2  = (Move id L)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 > y2 && (x1-x2) >= (y1-y2) = (Move id D)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 > y2  = (Move id R)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 > y2 && (x2-x1) >= (y2-y1) = (Move id U)
                                 | nxtWall gcoords m orient && x1 < x2 && y1 > y2  = (Move id R)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 < y2 && (x1-x2) >= (y2-y1) = (Move id D)
                                 | nxtWall gcoords m orient && x1 > x2 && y1 < y2  = (Move id L)
                                 | x1 == x2 && y1 > y2 && (y1-y2) <= 6 && checkForWalls c R m cp = (Move id R)
                                 | x1 == x2 && y1 < y2 && (y2-y1) <= 6 && checkForWalls c L m cp = (Move id L)
                                 | x1 < x2 && y1 == y2 && (x2-x1) <= 6 && checkForWalls c U m cp = (Move id U)
                                 | x1 > x2 && y1 == y2 && (x1-x2) <= 6 && checkForWalls c D m cp = (Move id D)
                                 | otherwise = (Move id orient)
                                     where orient = getPlayerOrientation (idPlayer x id)
                                           gcoords = getPlayerCoords (idPlayer x id)



-- | Função final que utiliza a função auxiliar anterior para dar um determinado ordem (Play) ao fantasma com o determinado ID, para ir atrás do Pacman.
chaseMode :: State -> Int -> Play
chaseMode s id = coordsChase s id (pacCoords s) (ghostIDCoords s id)


-- | Função auxiliar que dado um State, um ID, as coordenadas do Pacman e do Ghost com o determinado ID, decide a jogada a fazer para o Ghost fugir do Pacman.
coordsScatter :: State  -- ^ Estado do jogo.
              -> Int    -- ^ ID de um determinado fantasma.
              -> Coords -- ^ Coordenadas do Pacman.
              -> Coords -- ^ Coordenadas do fantasma com o determinado ID.
              -> Play   -- ^ Play a executar pelo fantasma com o respetivo ID com a intenção de fugir do Pacman.
coordsScatter s@(State m x l) id (x1,y1) (x2,y2) 
                                   | nxtWall gcoords m orient && orient == R = (Move id D)
                                   | nxtWall gcoords m orient && orient == D = (Move id L)
                                   | nxtWall gcoords m orient && orient == L = (Move id U)
                                   | nxtWall gcoords m orient && orient == U = (Move id R)
                                   | otherwise = (Move id orient)
                                       where orient = getPlayerOrientation (idPlayer x id)
                                             gcoords = getPlayerCoords (idPlayer x id)

-- | Função final que utiliza a função auxiliar anterior para dar um determinado ordem(Play) ao fantasma com o determinado ID, para fugir do Pacman.
scatterMode :: State -> Int -> Play
scatterMode s id = coordsScatter s id (pacCoords s) (ghostIDCoords s id)



-- | Função final adaptada para funcionar na Tarefa 4.
ghostPlayAux :: [Player] -> State -> [Play]
ghostPlayAux [] (State m z l) = []
ghostPlayAux (x:xs) s@(State m z l) | isPac x = ghostPlayAux xs (State m z l)
                                    | isDeadGhost x = scatterMode s (getPlayerID x) : ghostPlayAux xs (State m xs l)
                                    | otherwise = chaseMode s (getPlayerID x) : ghostPlayAux xs (State m xs l)



-- | Função final que dado um State, retorna o conjunto de Plays a fazer pelos jogadores do State.
ghostPlay :: State -> [Play]
ghostPlay (State m [] l) = []
ghostPlay s@(State m (x:xs) l) | isPac x = ghostPlay (State m xs l)
                               | isDeadGhost x = scatterMode s (getPlayerID x) : ghostPlay (State m xs l)
                               | otherwise = chaseMode s (getPlayerID x) : ghostPlay (State m xs l)

