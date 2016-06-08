-- prog string texto.txt

import System.Environment

main = do arg <- getArgs
          arquivo <- last arg
          txt <- readFile(first arg)
          putStr (show (palavras txt))
          
substituir e txt = 
   where
      linhas txt = lines
