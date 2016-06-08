{-
 Versão criada no Sábado, atualização no SHOW da árvore!
import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman


instance Show Huffman where
   show (Folha c i) = show c ++ ":" ++ show i
   show (No i h h') = show i++ " ->(  "++(show h)++" , "++(show h')++"  )"

freq [] = []
freq a@(x:xs) = Folha x (contar x a):freq (remover x xs)
   where
      remover _ [] = []
      remover e (x:xs) = if e == x then remover e xs else x:remover e xs
      contar e [] = 0
      contar e (x:xs) = if e == x then 1 + contar e xs else contar e xs

-- Criar com 2 elementos
criar (x:y:[]) = No (getf x + getf y) (pegaMenor) (pegaMaior)
  where
    getf (Folha _ a) = a
    getf (No a _ _) = a
    pegaMenor = if getf x < getf y then x else y
    pegaMaior = if getf x < getf y then y else x
    
-- Criar com mais elementos
criar b = criar ((No ((getf (primeiro a)) + (getf (segundo a))) (primeiro a) (segundo a)):remover a)
   where
      getf (Folha _ e) = e
      getf (No e _ _) = e
      comp x y = compare (getf x) (getf y)
      ordenar = sortBy comp b
      a = ordenar
      primeiro (x:y:xs) = x
      segundo (x:y:xs) = y
      remover (x:y:xs) = xs

codifica (Folha c f) = [(c,"")]
codifica (No _ a b) = (conc (codifica a) "0") ++ (conc (codifica b) "1")
   where
      conc [] _ = []
      conc (x:xs) e = (fst x, e ++ snd x): conc xs e

comprimir [] cod = []
comprimir (x:xs) cod = findB x cod ++ comprimir xs cod
   where
      findB e (x:xs) = if fst x == e then snd x else findB e xs

tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs



descomprimir :: Huffman -> String -> Huffman -> String

descomprimir (No f a b) (x:[]) _ = if x == '1' then [pegarC b] else [pegarC a]
  where
    pegarC (Folha c _) = c

descomprimir (Folha c _) (x:[]) _ = c:[]
descomprimir (No f a b) (x:xs) arv = if x == '0' then descomprimir a xs arv else descomprimir b xs arv
descomprimir (Folha c _) (x:xs) arv = c : descomprimir arv (x:xs) arv

desc t a = descomprimir a t a
comp t = comprimir t (codifica(criar(freq t)))
arv t = criar(freq t)
-}

import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman


instance Show Huffman where
--   show (Folha c i) = show c ++ ":" ++ show i
--   show (No i h h') = show i++ " ->(  "++(show h)++" , "++(show h')++"  )"
     show (Folha c i) = show c ++ "->" ++ show i ++ "\n"
     show (No i h h') = "(root)"++show i ++":\n" ++ mostrar h 1 0 ++ mostrar h' 1 1
        where
           mostrar (No f q q') n c = tabs n (n-1)++"("++ show c ++")"++ show f ++":\n" ++ mostrar (q) (n+1) 0 ++ "" ++ mostrar (q') (n+1) 1 ++"\n" 
           mostrar folha n c = tabs n (n-1) ++"("++ show c ++")"++ show folha
           tabs n k
              | n == 0 = ""
              | n > k  = tabs (n-1) k ++ " |__ " 
              | n <= k = tabs (n-1) k ++ "     " 
   

freq [] = []
freq a@(x:xs) = Folha x (contar x a):freq (remover x xs)
   where
      remover _ [] = []
      remover e (x:xs) = if e == x then remover e xs else x:remover e xs
      contar e [] = 0
      contar e (x:xs) = if e == x then 1 + contar e xs else contar e xs

-- Criar com 2 elementos
criar (x:y:[]) = No (getf x + getf y) (pegaMenor) (pegaMaior)
  where
    getf (Folha _ a) = a
    getf (No a _ _) = a
    pegaMenor = if getf x < getf y then x else y
    pegaMaior = if getf x < getf y then y else x
    
-- Criar com mais elementos
criar b = criar ((No ((getf (primeiro a)) + (getf (segundo a))) (primeiro a) (segundo a)):remover a)
   where
      getf (Folha _ e) = e
      getf (No e _ _) = e
      comp x y = compare (getf x) (getf y)
      ordenar = sortBy comp b
      a = ordenar
      primeiro (x:y:xs) = x
      segundo (x:y:xs) = y
      remover (x:y:xs) = xs

codifica (Folha c f) = [(c,"")]
codifica (No _ a b) = (conc (codifica a) "0") ++ (conc (codifica b) "1")
   where
      conc [] _ = []
      conc (x:xs) e = (fst x, e ++ snd x): conc xs e

comprimir [] cod = []
comprimir (x:xs) cod = findB x cod ++ comprimir xs cod
   where
      findB e (x:xs) = if fst x == e then snd x else findB e xs

tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs



descomprimir :: Huffman -> String -> Huffman -> String

descomprimir (No f a b) (x:[]) _ = if x == '1' then [pegarC b] else [pegarC a]
  where
    pegarC (Folha c _) = c

descomprimir (Folha c _) (x:[]) _ = c:[]
descomprimir (No f a b) (x:xs) arv = if x == '0' then descomprimir a xs arv else descomprimir b xs arv
descomprimir (Folha c _) (x:xs) arv = c : descomprimir arv (x:xs) arv

desc t a = descomprimir a t a
comp t = comprimir t (codifica(criar(freq t)))
arv t = criar(freq t)


