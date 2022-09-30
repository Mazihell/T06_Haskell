{-# LANGUAGE ParallelListComp #-}
module Main where

--1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado.
divisoresden :: Int -> [Int]
divisoresden d = [x | x <- [1,2..d], d `mod` x == 0]

--2. Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a ocorrência de um caractere específico, em uma string dada.
contaCaractere :: String -> Char -> Int
contaCaractere s c = sum[1 | x <- s, x == c]

-- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, 
--que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada.
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo d = [x * 2 | x <- d, x > 0]

--4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os 
--lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado.

--5. Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número. Usando List Comprehension escreva uma função, 
--chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função 
--que devolve uma lista dos divisores de um número dado.

menores :: Int -> [Int]
menores n = [x  | x <- divisoresden n, x < n]
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n | (sum(menores n)) == n = menores n | otherwise = []

--6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto 
--escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis.

produtoEscalar :: [Int] -> [Int] -> [Int]
produtoEscalar a b = [x * y | y <- b | x <- a]

--7. Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva
--uma lista contendo os n primeiros números primos a partir do número 2.

--8. Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva
--uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um
--determinado número dado. Observe que estes números podem ser bem grandes.

paresOrdenados :: Int -> [(Int,Int)]
paresOrdenados n = [(x ^ 2, y ^ 3) | x <- [1,2..n] | y <- [1,2..n]]



main :: IO ()
main = do

putStrLn $ ("Func.1: entrada 10 resultado " ++ show(divisoresden 10))
putStrLn $ ("Func.2: entrada 'andando pela rua encontrei a ana' 'a' resultado " ++ show(contaCaractere "andando pela rua encontrei a ana" 'a'))
putStrLn $ ("Func.3: entrada [-3, -5, -1 , 0, 2, 3, 4] resultado " ++ show(dobroNaoNegativo [-3, -5, -1 , 0, 2, 3, 4]))
putStrLn $ ("Func.5: entrada 28 resultado " ++ show(numerosPerfeitos 28))
putStrLn $ ("Func.6: entrada [1,2,3] [3,2,2] resultado " ++ show(produtoEscalar [1,2,3] [3,2,2]))
putStrLn $ ("Func.8: entrada 10 resultado " ++ show(paresOrdenados 10))
