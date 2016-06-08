import Data.List

data Huffman = Folha Char Int | No Int Huffman Huffman deriving Show


frequencia :: String -> [Huffman]

frequencia [] = []
frequencia t@(x:xs) = Folha (x)(contar x t):frequencia(remover x xs)
  where
    contar _ [] = 0
    contar c (x:xs)
      | c == x = 1 + contar c xs
      | True = contar c xs
    remover _ [] = []
    remover c (x:xs)
      | c == x = remover c xs
      | True = x:remover c xs
      


criarArvore t = make(ordenar t)
  where
    make a@(x:xs) = No ((pegarFreq x)+(pegarFreq(pegarPrim xs))) ()
    pegarPrim (x:_) = x
    pegarFreq (Folha _ a) = a
    pegarFreq (No a _ _) = a
    ordenar = sortBy sortLogic
    sortLogic a b = compare (pegarFreq a) (pegarFreq b)
    
