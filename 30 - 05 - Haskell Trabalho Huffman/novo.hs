-- Este programa é protegido pelas leis dos homens e de Deus, afinal, só ele sabe o que essa merda faz
--Marlon
--Tiago 
--UDESC????será?

import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import qualified Data.Binary.Get as G

import System.Environment
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

comp t = comprimir t (codifica(criar(freq t)))

pintoList [] = []
pintoList a = (toBin(pega)):pintoList(remover 8 a)
   where
     remover 0 a = a
     remover n [] = []
     remover n (x:xs) = remover (n-1) xs

     toBin [x] = if x == '1' then 1
                             else 0
     toBin (x:xs) = if x == '1' then 2^(tamanho xs) + (toBin xs)
                                else (toBin xs)
     pega = take 8 (a ++ repeat '0' )

listtoBin [] = ""
listtoBin (x:xs) = completa (8 - (tamanho (toBin x))) (reverse (toBin x)) ++ listtoBin xs
   where
      completa 0 s = s
      completa n s = "0" ++ completa (n-1) s
      toBin 1 = "1"
      toBin 0 = "0"
      toBin x = if mod x 2 == 1 then "1" ++ (toBin (div x 2))
                                else "0" ++ (toBin (div x 2)) 
-- ESCREVER
main = do
   arg <- getLine
   text <- readFile(arg)
   --let buff = escrever text
   let bin = P.runPut (escrever text)
   L.writeFile (arg++".bin") bin
   --putStrLn text
   putStrLn "Ok"
   return ()
   
escrever txt = do
   wtam(tamanho (comp (txt)))
   wtam(tamanho (freq(txt)))
   wtam(tamanho (pintoList(comp (txt))))
   wfreq (freq (txt))
   wbin (pintoList(comp (txt)))
   
wtam tam = do
   P.putWord32be (toEnum tam)

wfreq ((Folha c i):[]) = do
   P.putWord8 (I.c2w c)
   P.putWord32be (toEnum i)

wfreq ((Folha c i):xs) = do
   P.putWord8 (I.c2w c)
   P.putWord32be (toEnum i)
   wfreq xs

wbin [] = P.flush
wbin (x:xs) = do
  P.putWord32be (toEnum x)
  wbin xs

-- LER

main1 = do
   arg <- getLine
   text <- L.readFile arg
   let (tam, a, b, c, d) = G.runGet getTiagoAll text
--   a = Tamanho de registros
--   b = Tamanho da palavra
--   c = Duplas da Frequencia
--   d = Lista de Inteiros
   putStrLn (show a)
   putStrLn (show b)
   printLegal c
   printLegal2 d
   --let bin = listtoBin(d)
   --let arv = makeArvore(c)
   --let arvc = criar(arv)
   --putStrLn (descomprimir arvc bin arvc)
   let e = listtoBin d
   let arvore = makeArvore c
   let fuckoff = (take (fromEnum tam) e)
   let saida = desc (fuckoff) (criar arvore)
   putStrLn saida
   writeFile "out.txt" saida
   putStrLn "Ok"


desc t a = descomprimir a t a


makeArvore [] = []
makeArvore ((a,b):xs) = (Folha (I.w2c a) (fromEnum b)):makeArvore xs 

printLegal [] = do return ()
printLegal (x:xs) = do putStr (show (snd x)); putStr " - "; putStrLn (show  (fst x)); printLegal xs

printLegal2 [] = do return()
printLegal2 (x:xs) = do putStr(show x ++ "\n"); printLegal2 xs

--letBin 0 = ""
{-
letBin n = do
  if n == 0 then return [] else do
   c <- G.getWord8
   return (c:letBin (n-1))
-}

letReg n = do
  if n == 0 then return [] else do 
   c <- G.getWord8
   f <- G.getWord32be
   rs <- letReg(n-1)
   return ( (c,f):rs )
   
getNum = do
    num_ <- G.getWord32be       
    return num_

getTiagoAll = do
  num0 <- G.getWord32be
  num1 <- G.getWord32be 
  num2 <- G.getWord32be
  duplas <- letReg num1
  lista <- letList num2
  return (num0, num1, num2, duplas, lista)

letList n = do
   if n == 0 then return [] else do
   f <- G.getWord32be
   rs <- letList(n-1)
   return (f: rs) 
