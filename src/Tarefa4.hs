{- | 

= Introdução

O objetivo desta tarefa foi calcular o efeito da passagem de um instante de tempo num estado do jogo. 
A princípio esta tarefa pareceu assustadora, uma vez que introduz o conceito de passagem de tempo, mas com a ajuda do professor e alguma discussão entre colegas, conseguimos desenvolver estratégias para começarmos a fazer a tarefa.

= Objetivos

Nesta tarefa, foi usada uma unidade temporal de 250 ms por iteração. Usamos uma função que, dado um State e uma lista de Players, vai determinar a melhor jogada a ser efetuada para todos os players, analisando esse mesmo State.

Inicialmente criamos uma função que só fazia com que todos os Players do State andassem para a direita, como sugestão do professor.
Mais tarde, após a conclusão da Tarefa 5, melhoramos essa função implementando a obrigatoriedade de fazer o conjunto de jogadas que foram determinadas na Tarefa5 e fazer andar o Pacman na direção que ele tinha, sendo essa direção alterada pelas setas do teclado através da função updateControlledPlayer da Main.

Para que os fantasmas que estivessem no estado Dead andassem a metade da velocidade do Pacman, fizemos uma função em que quando o Step (número de iterações que o jogo já passou) é par, só jogam os Players que não são Dead Ghosts.
Os restantes Players não são afetados por essa restrição, tendo assim, o conceito de velocidade, sido implementado da maneira que se o Player for um Ghost Dead só jogará de 500 ms em 500 ms, enquanto que os outros jogam de 250 ms em 250 ms (DefaultDelayTime).
De seguida, para que haja a redução do TimeMega de um Pacman, foi introduzida uma função que a cada 4 iterações passadas reduziria em 1 o TimeMega, ou seja a cada segundo que passava era reduzido em 1 o valor do TimeMega dum Pacman Mega.
Consequente a essa redução de timeMega, alteramos certos aspetos da Tarefa2, modificando e criando novas funções que faria um Pacman no estado Mega e com MegaTime 0, voltar ao normal, e sem nenhum Pacman em modo Mega, transformar-se-iam de volta todos os fantasmas Dead, em Alive.

Para podermos verificar o bot da Tarefa 6, criamos a função jogaListab que, tal como na jogaLista (original), recebe as jogadas dos Ghosts para atualizar o State, mas o Pacman é controlado pelo bot da Tarefa 6 e não pelos inputs que realizamos através das setas do teclado.
Atualmente a função passTime está configurada para que o Pacman não seja controlado pelo bot, mas sim pelas setas do teclado.

Por fim, com todas estas novas funções em consideração a função final passTime foi criada, sendo assim concretizado o objetivo de calcular o efeito de passagem do tempo num State.

= Discussão e conclusão

Nesta tarefa foi atingido o seu principal objetivo, podendo ser verificada a passagem do tempo no mapa e nos jogadores através das funções criadas, havendo atualização do estado de jogo a cada iteração.

-}

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import Tarefa6  -- para o bot da Tarefa 6 jogar

defaultDelayTime = 250 -- 250 ms


-- | Dado uma lista de Players e um State, vai fazer um play para todos os jogadores utilizando as funções da seguinte tarefa, que analisam o State e determinam a melhor jogada a efetuar por parte dos players fantasmas, e para o Pacman, faz com que ele se mova no sentido da sua orientação.
jogaLista :: [Player] -> State -> State
jogaLista [] s = s
jogaLista p@(x:xs) s = let pac = thePacman p
                           jogadas = ghostPlayAux p s
                        in play (Move (getPlayerID pac) (getPlayerOrientation pac)) (plays jogadas s)


-- | Função que dado um conjunto de Plays e um State, aplica todas as Plays ao State.
plays :: [Play] -> State -> State 
plays [] s = s
plays (x:xs) s = plays xs (play x s)


-- | Dado uma lista de Players, retorna uma lista de Players, sem os Ghosts Dead.
arentDGhost :: [Player] -> [Player]
arentDGhost [] = []
arentDGhost ((Ghost (GhoState (a,b,c,d,e,f) Dead)):xs) = arentDGhost xs
arentDGhost (p@(Ghost (GhoState (a,b,c,d,e,f) Alive)):xs) = p : arentDGhost xs
arentDGhost (p@(Pacman (PacState (a,b,c,d,e,f) g h i)):xs) = p : arentDGhost xs 


-- | Função que faz os players com metade da velocidade, fazerem uma jogada a cada duas iterações.
passTimeAux :: Int -> State -> State
passTimeAux n s@(State m x l) | even n = jogaLista (arentDGhost x) s
                              | otherwise = jogaLista x s 

-- | Dado uma lista de players devolve um lista de players com a redução em 1 segundo da duração do TimeMega, de todos os Pacmans que estão no estado Mega.
reduceMegaSec :: [Player] -> [Player]
reduceMegaSec [] = []
reduceMegaSec ((Pacman (PacState (a,b,c,d,e,f) g h Mega)):xs) = (Pacman (PacState (a,b,c,d,e,f) (g-1) h Mega)) : reduceMegaSec xs
reduceMegaSec (x:xs) = x : reduceMegaSec xs

-- | Função que aplica na lista de jogadores de um /State/, a função anterior que reduz em um segundo a duração do TimeMega dos Pacman Mega.
reduceMega :: State -> State
reduceMega (State m x l) = (State m (reduceMegaSec x) l) 


-- | Função que diz se um número é multiplo de 4, utilizada para detetar a passagem de 1 segundo (=250*4 ms).
quadruplo :: Int -> Bool
quadruplo x | mod x 4 == 0 = True
            | otherwise = False


-- | Função final que aplica a passagem de tempo.
passTime :: Int -> State -> State
passTime n s | quadruplo n = reduceMega (passTimeAux n s) 
             | otherwise = passTimeAux n s




-- | Função utilizada para testar o bot da Tarefa 6, dando as jogadas dos fantasmas da Tarefa 5, mas com o Pacman a ser controlado pelo bot da Tarefa 6 
jogaListab :: [Player] -> State -> State
jogaListab [] s = s
jogaListab p@(x:xs) s = let pac = thePacman p
                            jogadas = ghostPlayAux p s
                        in play (transPlay (bot (getPlayerID pac) s )) (plays jogadas s)


-- | Transforma um Maybe Play num Play.
transPlay :: Maybe Play -> Play
transPlay (Just p) = p

