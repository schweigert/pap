
main = do arq <- getLine
          txt <- readFile arq
          putStr (show (palavras txt))
          
palavras texto = contar(getWords(tratar(texto)))
   where
      tratar [] = []
      tratar (x:xs)
         | (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') = x:tratar xs
         | x == ' ' = x:tratar xs
         | x == '\n' = ' ':tratar xs
         | otherwise = tratar xs
      getWords a = words a
      contar [] = 0
      contar (x:xs) = 1 + contar xs
