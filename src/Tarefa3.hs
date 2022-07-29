module Tarefa3 where

import Types
import Tarefa1 -- Para poder usar o generateMaze nos testes.


-- | É uma função auxiliar que transforma um corredor de um labirinto, peça por peça, em uma lista de tuplos com o número 1 e a respetiva peça.
basicCorr :: Corridor       -- ^ Lista de peças que vão ser transformadas.
          -> [(Int, Piece)] -- ^ Lista de tuplos na forma [(1, /Piece/)], em que cada tuplo corresponde a uma peça.
basicCorr [] = []
basicCorr (x:xs) | x == Wall = ((1,Wall) : basicCorr xs)
                 | x == Food Little = ((1,Food Little) : basicCorr xs)
                 | x == Empty = ((1, Empty) : basicCorr xs)
                 | x == Food Big = ((1,Food Big) : basicCorr xs)


-- | Função que junta os números, respetivos a peças iguais consecutivas, numa lista de tuplos [(/Int/, /Piece/)], dando uma lista mais compacta de tuplos (/Int/, /Piece/).
gatherBasic :: [(Int, Piece)] -- ^ Lista de tuplos não otimizadamente compacta.
            -> [(Int, Piece)] -- ^ Lista de tuplos com os padrões horizontais compactados.
gatherBasic [x] = [x]
gatherBasic ((n,p):(n1,p1):xs) | p==p1 = gatherBasic ((n+n1,p):xs)
                               | otherwise = (n,p) : gatherBasic ((n1,p1):xs)


-- | Função que transforma, primeiramente, um corredor em uma lista do tipo (/Int/, /Piece/), de seguida dá-se a compactação dessa lista através da função gatherBasic enunciada anteriormente, transformando cada lista de tuplos compactada numa instrução do tipo Instruct (/Int/, /Piece/), para cada corredor do labirinto, dando no final um conjunto de instruções :: /Instructions/. 
basic :: Maze         -- ^ Conjunto de corredores que irão ser compactados num padrão horizontal.
      -> Instructions -- ^ Instruções do labirinto compactas num padrão horizontal.
basic [] = []
basic (x:xs) = [Instruct (gatherBasic (basicCorr x))] ++ basic xs 


-- | Função auxiliar que calcula a lista de posições em que um dado elemento ocorre numa lista.
elemIndices :: Eq a => a     -- ^ Elemento que queremos verificar se ocorre na lista.
                    -> [a]   -- ^ Lista onde queremos verificar a ocorrência do elemento.
                    -> [Int] -- ^ Lista de posições em que o elemento ocorre na lista.
elemIndices _ [] = []
elemIndices n (x:xs) | n==x = 0 : map (+1) (elemIndices n xs)
                     | otherwise = map (+1) (elemIndices n xs)


-- | Função auxiliar da função poeRepeats (função seguinte), que substitui a instrução (ordenada pelo inteiro recebido na 2ª entrada) numa lista de instruções, por uma instrução do tipo /Repeat Int/ ,cujo /Int/ é o fornecido no primeiro argumento. 
poeRepeatsaux :: Int          -- ^ Inteiro n, que vai estar na consituição da /Instruction/, /Repeat/ n, que vai subsituir uma determinada /Instruction/ numa lista.
              -> Int          -- ^ Indice da instrução que pretendemos substituir.
              -> Instructions -- ^ Lista de /Instruction/ que queremos alterar.
              -> Instructions -- ^ Lista de /Instruction/ com a substituição pretendida. 
poeRepeatsaux _ _ [] = [] 
poeRepeatsaux n p (x:xs) | p==0 = ((Repeat n):xs)
                         | otherwise = x : poeRepeatsaux n (p-1) xs


-- | Função que substitui as instruções (ordenadas pela lista de inteiros recebida no 2º argumento de entrada) de uma lista de instruções, por uma instrução do tipo /Repeat Int/, cujo /Int/ é o fornecido no primeiro argumento. 
poeRepeats :: Int          -- ^ Inteiro n, que vai estar na consituição da /Instruction/, /Repeat/ n, que vai subsituir certas /Instruction/ de uma lista.
           -> [Int]        -- ^ Indices das instruções que pretendemos subsituir.
           -> Instructions -- ^ Lista de /Instruction/ que queremos alterar.
           -> Instructions -- ^ Lista de /Instruction/ com a substituição desejada.
poeRepeats _ [] x = x
poeRepeats n (p:ps) x = poeRepeatsaux n p (poeRepeats n ps x)


-- | Função auxiliar à função seguinte, que recebe duas instruções, e que verifica se cada elemento da primeira /Instructions/, tem algum elemento igual à segunda /Instructions/, e no caso de haver, esses elementos repetidos (excetuando o primeiro) que estão na segunda lista, vão ser substituídos por uma instrução do tipo /Repeat/ n (em que n é o indice da primeira ocorrência desse elemento na primeira /Instructions/), através da funcão auxiliar anterior.
repeater :: Instructions -- ^ Primeira /Instructions/ em que vai ser verificado se cada elemento, tem algum igual à segunda /Instructions/.
         -> Instructions -- ^ Segunda /Instructions/, que vai sofrer as alterações caso hajam elementos da primeira /Instructions/ iguais a elementos desta segunda /Instructions/.
         -> Instructions -- ^ /Instructions/ com as alterações feitas à segunda /Instructions/.
repeater [] m = m
repeater (x:xs) m | elem x m = repeater xs (poeRepeats (head (elemIndices x m)) (tail(elemIndices x m)) m)
                  | otherwise = repeater xs m

-- | Função que utiliza a anterior, para verificar elementos repetidos num mesmo labirinto.
simpRepeater :: Instructions -- ^ /Instructions/ na qual vai ser verificado se existem elementos iguais ao longo da lista e caso haja, vão ser substituidos (excetuando o primeiro elemento dos repetidos) por uma /Instruction/ do tipo /Repeat Int/, em que o /Int/ é o indice do primeiro elemento dos repetidos.  
             -> Instructions -- ^ /Instructions/ final com os padrões verticais compactados.
simpRepeater m = repeater m m


-- | Função final que compacta ao máximo um labirinto em instruções.
compactMaze :: Maze         -- ^ Labirinto que irá ser compactado.
            -> Instructions -- ^ /Instructions/ compactadas que correspondem ao labirinto.
compactMaze m = simpRepeater (basic m)



-- * Funções-teste.

-- | Casos de labirintos para a função compactMaze.
m2 = [(generateMaze 15 10 43),(generateMaze 20 15 35),(generateMaze 17 11 4)]

-- | Testa os casos para a função compactMaze.
testItT3 :: [Maze] -> [Instructions]
testItT3 [] = []
testItT3 (x:xs) = compactMaze x : testItT3 xs


-- | Função auxiliar que faz o somatório do número de peças de uma lista de (/Int/, /Piece/).
somatorio :: [(Int,Piece)] -> Int
somatorio [] = 0
somatorio ((n,p):xs) = n + somatorio xs


-- | Testa se a Instruction é válida (ou seja, se o somatório do número de peças é igual à largura) para um determinado labirinto.
validInstruction :: Maze -> Instruction -> Bool
validInstruction m@(x:ms) (Instruct p) 
                   | length x == somatorio p = True
                   | otherwise = False
validInstruction m (Repeat n) = True


-- | Testa se as /Instructions/ são válidas para um determinado labirinto.
validInstructions :: Maze -> Instructions -> Bool
validInstructions m [] = True
validInstructions m (i:is) | validInstruction m i = validInstructions m is
                           | otherwise = False


-- | Descompacta uma instrução num corredor.
decCorridor :: Instruction -> Corridor
decCorridor (Instruct []) = [] 
decCorridor (Instruct ((n,p):xs)) | n==0 = decCorridor (Instruct xs)
                                  | otherwise = p : decCorridor (Instruct ((n-1,p):xs))


-- | Descompacta /Instructions/ básicas num labirinto.
decompact :: Instructions -> Maze
decompact [] = []
decompact (i:is) = decCorridor i : decompact is
