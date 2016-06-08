-- Mônadas IO

ola = do putStr "Qual o seu nome?\n"
         nome <- getLine
         putStr ("Ola "++nome++"\n")

	
ola' = putStr "Qual o seu nome?\n" >> getLine >>=(\nome -> putStr ("Ola "++nome++"\n"))

{- 
    class Monada
        (>>=):: m a -> (a -> m b) -> m b
        (>>) :: m a -> m b -> m b
        return :: a -> m a
-}

main2 = do putStr "Arquivo:"
           arq <- getLine
           txt <- readFile arq
           putStr (show txt)
