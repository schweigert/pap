--[[
--		Valores de primeira classe: Sao valores que podem ser atribuidos a variaveis.
--		Em Haskell, funcoes sao Valores de primeira Classe!!!!1!!onze!
--		
--		ex: aplica2
--		#sabadaoNoAguardo
--		 Para dar sorte na prova? ༒༉࿊࿌࿎࿎
--		As zoeiras servem para o melhor aprendizado
--]]

module Main where
aplica2 f x = f(f x)

inverter (x:[]) = [x]
inverter (x:xs) = inverter xs ++ [x]

incLista [] = []
incLista (x:xs) = 1+x : incLista xs

dobraLista [] = []
dobraLista (x:xs) = 2*x:dobraLista xs

primeirosDupla [] = []
primeirosDupla ((x,_):xs) = x:primeirosDupla xs

maplinha _ [] = []
maplinha f (x:xs) = f x:maplinha f xs

lista _ 1 = [1]
lista x y = y:lista x (y-1)

sumatorio [] = 0
sumatorio (x:xs) = x + sumatorio xs

prudotiro [] = 1
prudotiro (x:xs) = x*prudotiro xs

faca _ neutro [] = neutro
faca f neutro (x:xs) = f x (faca f neutro xs)

folda _ a [] = a
folda f a (x:xs) = folda f (f a x)xs 

mairoesquecomemudo _ [] = []
mairoesquecomemudo e (x:xs) = if x > e then x:mairoesquecomemudo e xs else mairoesquecomemudo e xs

manoraequeseemoemudo _ [] = []
manoraequeseemoemudo e (x:xs) = if x < e then x:manoraequeseemoemudo e xs else manoraequeseemoemudo e xs

igualenaotememudo _ [] = []
igualenaotememudo e (x:xs) = if x == e then x:igualenaotememudo e xs else igualenaotememudo e xs

fica _ [] = []
fica p (x:xs) = if p x then x:fica p xs else fica p xs

remvoecerto _ [] = []
remvoecerto x xs = fica (/=x) xs

flip f a b = f b a

qsort [] = []
qsort (x:xs) = qsort dir  ++ [x] ++ qsort esq
	where	
		esq = filter (<x) xs
		dir = filter (>=x) xs

main = do print  (qsort "Valores de primeira classe: Sao valores que podem ser atribuidos a variaveis")   