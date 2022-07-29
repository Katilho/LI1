{- |

= Introdução

Esta tarefa consistiu em desenvolver um bot que fosse capaz de jogar sozinho sem nenhum input humano.


= Objetivos

O objetivo desta tarefa era dar jogadas ao Pacman, tendo em conta o estado do jogo e do mapa. Definimos como objetivos ir atrás do fantasmas mortos, fugir de fantasmas vivos, e quando estiver perto, comer uma Big Food.
Como objetivo principal estabelecemos que era fugir a fantasmas vivos, priorizando estar vivo a obter pontos, para tal o bot verifica se existe algum fantasma vivo a aproximar-se dele e caso haja, ele tenta ir na direção oposta para fugir do perigo.
Secundariamente, ou seja, caso não houvesse perigo por perto, o bot iria atrás de fantasmas mortos, que dão uma maior pontuação e eliminam a ameaça do mapa (temporariamente).
Caso não detete nenhuma destas circunstâncias, ele irá tentar ir atrás de comidas grandes para conseguir aumentar a sua pontuação e forçar os ghosts a ficarem Dead. 

= Discussão e conclusão

Apesar de não ser um bot extremamente inteligente, com uma inteligência artificial espetacular, dá para ver que é capaz de cumprir os seus objetivos fugindo do perigo e indo atrás de oportunidades para pontuar.
Esta tarefa foi uma tarefa trabalhosa, mas, tal como na Tarefa 5, é muito satisfatório ver o código que se escreveu ser posto em prática ao testar e ver os fantasmas e o Pacman uns atrás dos outros, aplicando as estratégias que desenhamos para eles.

-}

module Tarefa6 where

import Types
import Tarefa5


-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades à esquerda.
neargL :: Coords -> [Coords] -> Bool
neargL _ [] = False
neargL (x,y) ((x1,y1):xs) | (y-y1) <= 4 && y1 < y = True
                          | otherwise = neargL (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades à direita.
neargR :: Coords -> [Coords] -> Bool
neargR _ [] = False
neargR (x,y) ((x1,y1):xs) | (y1-y) <= 4 && y < y1 = True
                          | otherwise = neargR (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades em cima.
neargU :: Coords -> [Coords] -> Bool
neargU _ [] = False
neargU (x,y) ((x1,y1):xs) | (x-x1) <= 4 && x1 < x = True
                          | otherwise = neargU (x,y) xs

-- | Função auxiliar que verifica as coordenadas do Pacman e dos fantasmas Alive, retornando True se houver algum fantasma Alive perto, a 4 unidades em baixo.
neargD :: Coords -> [Coords] -> Bool
neargD _ [] = False
neargD (x,y) ((x1,y1):xs) | (x1-x) <= 4 && x < x1 = True
                          | otherwise = neargD (x,y) xs



-- | Função auxiliar que dado um State, um Int, e as coordenadas do Pacman no labirinto, devolve a jogada a fazer pelo bot.
coordsMPl :: State -> Int -> Coords -> Maybe Play
coordsMPl s@(State m p l) id c@(x,y) | c == midCoord m = Just (Move id U)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R && nxtWall c m U = Just (Move id D)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R && nxtWall c m D = Just (Move id U)
                                     | neargL c (ghostsAlCoords s) && nxtWall c m R = Just (Move id D)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L && nxtWall c m U = Just (Move id D)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L && nxtWall c m D = Just (Move id U)
                                     | neargR c (ghostsAlCoords s) && nxtWall c m L = Just (Move id D)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D && nxtWall c m L = Just (Move id R)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D && nxtWall c m R = Just (Move id L)
                                     | neargU c (ghostsAlCoords s) && nxtWall c m D = Just (Move id R)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U && nxtWall c m L = Just (Move id R)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U && nxtWall c m R = Just (Move id L)
                                     | neargD c (ghostsAlCoords s) && nxtWall c m U = Just (Move id R)
                                     | neargL c (ghostsAlCoords s) && neargR c (ghostsAlCoords s)    = Just (Move id U)
                                     | neargU c (ghostsAlCoords s) && neargD c (ghostsAlCoords s)    = Just (Move id R)
                                     | neargL c (ghostsAlCoords s) = Just (Move id R)
                                     | neargR c (ghostsAlCoords s) = Just (Move id L)
                                     | neargU c (ghostsAlCoords s) = Just (Move id D)
                                     | neargD c (ghostsAlCoords s) = Just (Move id U)
                                     | neargL c (ghostsDCoords s)  = Just (Move id L)
                                     | neargR c (ghostsDCoords s)  = Just (Move id R)
                                     | neargU c (ghostsDCoords s)  = Just (Move id U)
                                     | neargD c (ghostsDCoords s)  = Just (Move id D)
                                     | nxtWall (x,y) m orient      = Just (Move id (turnR orient))
                                     | otherwise = chaseBF (x,y) (getPieceCoords (Food Big) s) id orient
                                         where orient = getPlayerOrientation (idPlayer p id)


-- | Função que dada uma orientação, dá a orientação seguinte, no sentido dos ponteiros do relógio.
turnR :: Orientation -> Orientation
turnR R = D
turnR D = L
turnR L = U
turnR U = R
turnR Null = Null


-- | Função que analisa as coordenadas do Pacman e dá a Play a realizar para ir atrás de uma das coordenadas das peças de comida grande que se encontra próximo. Caso não haja nenhuma peça próxima ele irá continuar continuar com a sua orientação. 
chaseBF :: Coords      -- ^ Coordenadas do Pacman
        -> [Coords]    -- ^ Conjunto de coordenadas das peças de comida grande
        -> Int         -- ^ ID do Pacman
        -> Orientation -- ^ Orientação atual do Pacman
        -> Maybe Play  -- ^ Play a fazer tendo em conta a análise das coordenadas
chaseBF (x,y) c id or | neargL (x,y) c = Just (Move id L)
                      | neargR (x,y) c = Just (Move id R)
                      | neargU (x,y) c = Just (Move id U)
                      | neargD (x,y) c = Just (Move id D)
                      | otherwise = Just (Move id or)



-- | Função final que recebe um ID e um State e utilizando as funções auxiliares anteriores, devolve a Play a fazer pelo Pacman bot.
bot :: Int -> State -> Maybe Play
bot id s@(State m p l) = coordsMPl s id cs
                    where cs = getPlayerCoords (idPlayer p id)
