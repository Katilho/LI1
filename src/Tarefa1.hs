module Tarefa1 where

import Types 
import System.Random

-- | Dada uma /seed/, retorna a lista dos n inteiros gerados aleatoriamente.
generateRandoms :: Int   -- ^ Número de inteiros a ser gerado.
                -> Int   -- ^ /seed/ de números aleatórios.
                -> [Int] -- ^ Lista dos números aleatórios. 
generateRandoms n seed = let gen = mkStdGen seed -- Cria um gerador aleatório.
                         in take n $ randomRs (0,99) gen -- Tira os primeiros n elementos de uma série infinita de números aleatórios entre 0 e 99.


-- | Converte uma lista numa lista de listas de tamanho n.
subList :: Int   -- ^ Tamanho n das sublistas.
        -> [a]   -- ^ Lista que vai ser dividida.
        -> [[a]] -- ^ Lista de listas de tamanho n resultante da divisão da lista pedida.
subList _ [] = []
subList n l = take n l : subList n (drop n l)


-- | Converte um número inteiro numa /Piece/.
convertPiece :: Int   -- ^ Inteiro entre 0 e 99 que vai ser convertido.
             -> Piece -- ^ Peça resultante da conversão.
convertPiece x
        | x == 30 = Food Big
        | x >= 0 && x<70 = Food Little
        | otherwise = Wall


-- | Converte um corredor numa /String/.
printCorridor :: Corridor -- ^ Conjunto de peças.
              -> String   -- ^ /String/ em que está representado simbolicamente o corredor.
printCorridor [] = "\n"
printCorridor (x:xs) = show x ++ printCorridor xs


-- | Converte um labirinto numa /String/.
mazeToString :: Maze   -- ^ Conjunto de corredores.
             -> String -- ^ /String/ em que está representado simbolicamente o labirinto.
mazeToString [] = []
mazeToString (x:xs) = printCorridor x ++ mazeToString xs


-- | Converte uma lista de inteiros num corredor.
convertCorridor :: [Int]    -- ^ Lista de inteiros que irão ser convertidos em peças.
                -> Corridor -- ^ Lista de peças convertidas.
convertCorridor [] = []
convertCorridor (x:xs) = convertPiece x : convertCorridor xs


-- | Converte uma lista de inteiros num labirinto.
convertMaze :: [[Int]] -- ^ Lista de listas de inteiros que irão ser convertidas em corredores.
            -> Maze    -- ^ Lista de corredores convertidos.
convertMaze [] = []
convertMaze (x:xs) = convertCorridor x : convertMaze xs


-- | Pede um número e constrói uma lista de paredes com esse tamanho.
justWalls :: Int      -- ^ Tamanho da lista pretendido.
          -> Corridor -- ^ Lista com tamanho n só com paredes.
justWalls 0 = []
justWalls n = Wall : justWalls (n-1)


-- | Substitui os limites superior e inferior do labirinto por paredes.
wallsUpDown :: Maze -- ^ Labirinto que vai sofrer as alterações. 
            -> Maze -- ^ Labirinto com paredes em cima e em baixo.
wallsUpDown (x:xs) = justWalls (length x) : init xs ++ [justWalls (length x)]


-- | Substitui os limites laterais do labirinto por paredes.
sideWalls :: Maze -- ^ Labirinto que vai sofrer as alterações.
          -> Maze -- ^ Labirinto com os limites laterais substituídos por paredes.
sideWalls [] = []
sideWalls (x:xs) = (Wall : (init (tail x)) ++ [Wall]) : sideWalls xs


-- | Substitui todos os limites do labirinto por paredes, utilizando as funções auxiliares anteriores.
closeMaze :: Maze -- ^ Labirinto que vai sofrer as alterações.
          -> Maze -- ^ Labirinto cercado por paredes.
closeMaze x = wallsUpDown (sideWalls x)


-- | Dá o número da posição do corredor central do labirinto.
getMiddle :: Maze -> Int
getMiddle x = (div (length x) 2)


-- | Substitui o primeiro e último elemento de um corredor por uma peça.
replaceInCorridorAB :: Maze  -- ^ Labirinto que vai sofrer as alterações.
                    -> Int   -- ^ Indice do corredor que vai ser alterado.
                    -> Piece -- ^ Peça pela qual queremos substituir.
                    -> Maze  -- ^ Labirinto com o primeiro e último elemento de um corredor substituído por uma determinada peça.
replaceInCorridorAB [] y p = []
replaceInCorridorAB (x:xs) 0 p = (replaceInList (length x - 1) p (replaceInList 0 p x)) : xs
replaceInCorridorAB (x:xs) y p = x : replaceInCorridorAB xs (y - 1) p


-- | Substitui num corredor o Indice 0 i (i :: /Int/), por uma determinada peça. 
replaceInList :: Int      -- ^ Indice no corredor que quer subsituir.
              -> Piece    -- ^ Peça pela qual quer substituir.
              -> Corridor -- ^ Corredor que vai sofrer as alterações.
              -> Corridor -- ^ Corredor com os indices 0 i substituídos por uma determinada peça. 
replaceInList i p [] = []
replaceInList 0 p (x:xs) = p : xs
replaceInList i p (x:xs) = x : (replaceInList (i-1) p xs)


-- | Faz o túnel no labirinto tendo em conta a sua altura, utilizando as funções auxiliares anteriores.
makeTunnel :: Maze -- ^ Labirinto em que vai ser construído o túnel.
           -> Maze -- ^ Labirinto com o túnel implementado.
makeTunnel x | odd (length x) = replaceInCorridorAB x mid Empty
             | otherwise = (replaceInCorridorAB (replaceInCorridorAB x mid Empty) (mid - 1) Empty)
                 where mid = getMiddle x 


-- | Função conjunta que é aplicada no generateMaze, que em primeiro fecha o labirinto e de seguida constrói o túnel.
mazeTunel :: Maze -- ^ Labirinto que vai ser fechado por paredes, sendo de seguida implementado o túnel.
          -> Maze -- ^ Labirinto rodeado por paredes mas com o túnel.
mazeTunel x = makeTunnel (closeMaze x)


-- | Função que nos dá o número da posicão da peça central dum corredor.
midCorridor :: Corridor -> Int
midCorridor x = div (length x) 2


-- | Constituição da parte central do labirinto que irá conter a casa dos fantasmas, tendo em conta as dimensões do labirinto.
constroiCasa :: Maze -- ^ Labirinto cujas dimensões vão ser analisadas.
             -> Maze -- ^ Constituição parte central que contém a casa dos fantasmas, relativamente ao labirinto pedido.
constroiCasa a@(x:xs)
   | even (length x) =  [
              take (midC - 5) midL0 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 5) midL0,
              take (midC - 5) midL1 ++ [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall, Empty] ++ drop (midC + 5) midL1,
              take (midC - 5) midL2 ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ drop (midC + 5) midL2,
              take (midC - 5) midL3 ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty] ++ drop (midC + 5) midL3,
              take (midC - 5) midL4 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 5) midL4      
                        ]

   | otherwise =  [  
               take (midC - 5) midL0 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 6) midL0,
               take (midC - 5) midL1 ++ [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall, Empty] ++ drop (midC + 6) midL1,
               take (midC - 5) midL2 ++ [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty] ++ drop (midC + 6) midL2,
               take (midC - 5) midL3 ++ [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Empty] ++ drop (midC + 6) midL3,
               take (midC - 5) midL4 ++ [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty] ++ drop (midC + 6) midL4 
                  ]
                        where midC = midCorridor x
                              midL0 = head (drop (getMiddle a - 2) a) -- primeiro elemento da parte central do labirinto.
                              midL1 = head (drop (getMiddle a - 1) a) -- segundo elemento da parte central do labirinto.
                              midL2 = head (drop (getMiddle a) a)     -- terceiro elemento da parte central do labirinto.
                              midL3 = head (drop (getMiddle a + 1) a) -- quarto elemento da parte central do labirinto.
                              midL4 = head (drop (getMiddle a + 2) a) -- quinto elemento da parte central do labirinto.


-- | Constrói a casa dos fantasmas no labirinto, tendo em conta a paridade da sua altura.
ghostHouse :: Maze -- ^ Labirinto no qual irá ser implementada a casa dos fantasmas.
           -> Maze -- ^ Labirinto com a casa dos fantasmas construída.
ghostHouse a | even (length a) = take (mid - 3) a ++ constroiCasa a ++ drop (mid + 2) a 
             | otherwise = take (mid - 2) a ++ constroiCasa a ++ drop (mid + 3) a
                  where mid = getMiddle a


-- | Função auxiliar para avaliar a validade do labirinto quanto às suas dimensões.
validM :: Int  -- ^ Largura do labirinto. 
       -> Int  -- ^ Altura do labirinto.
       -> Bool -- ^ /Bool/ que nos diz se o labirinto cumpre os requisitos mínimos das suas dimensões.
validM x y | x >= 15 && y >= 10 = True
           | otherwise = False


-- | Pede as dimensões do labirinto e a sua seed e faz um labirinto válido, dando erro caso as suas dimensões não cumpram os requisitos mínimos.
generateMaze :: Int  -- ^ Largura do labirinto.
             -> Int  -- ^ Altura do labirinto.
             -> Int  -- ^ /seed/ do labirinto.
             -> Maze -- ^ Labirinto válido com todos os requisitos estruturais e com as dimensões mínimas de 15x10.
generateMaze x y z | validM x y = mazeTunel (ghostHouse (convertMaze (subList x (generateRandoms (x*y) z))))
                   | otherwise = error "O labirinto não cumpre os requisitos mínimos para ser gerado (l>=15 e h>=10)"



-- | Mostra cada peça do labirinto nos seus respetivos símbolos atribuidos para uma visualização mais gráfica do labirinto.
imprimeMaze :: Maze -> IO ()
imprimeMaze l = do putStrLn ( mazeToString ( l ))


-- | Dado a sua largura, altura e uma /seed/, a função mostra o labirinto com essas características, de uma maneira mais gráfica para uma melhor visualização. 
genTest :: Int -> Int -> Int -> IO ()
genTest x y z = let gen = generateRandoms (x*y) z
                in imprimeMaze (mazeTunel (ghostHouse (convertMaze (subList x gen))))



-- * Funções-teste
-- | Casos de argumentos para a função generateMaze.
testCasesGenerateMaze :: [(Int,Int,Int)]
testCasesGenerateMaze = [(15,10,23),(20,10,45),(21,11,987),(16,13,765),(10,4,43),(50,50,50)]

-- | Testa os casos para a função generateMaze.
testItT1 :: [(Int,Int,Int)] -> [Maze]
testItT1 [] = []
testItT1 ((a,b,c):xs) = generateMaze a b c : testItT1 xs



-- | Verifica se a largura e comprimento do labirinto gerado cumprem os requisitos mínimos.
validMeasures :: Maze -> Bool
validMeasures m@(x:xs) | length x >= 15 && length m >= 10 = True
                       | otherwise = False


-- | Função auxiliar que verifica se um corredor tem só peças Wall.
checkCorr :: Corridor -> Bool
checkCorr [] = True
checkCorr c@(x:xs) | x==Wall = checkCorr xs
                   | otherwise = False


-- | Verifica se o primeiro e o último corredor estão com uma lista de Wall.
checkWallAB :: Maze -> Bool
checkWallAB [x] = checkCorr x
checkWallAB m@(x:xs) | checkCorr x = checkWallAB [last m]
                     | otherwise = False