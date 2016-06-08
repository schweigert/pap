import System.Environment

-- ghc nome_prog.hs
-- nome_prog 100

fat 0 = 1
fat n = fat(n-1)*n

main = do args <- getArgs
          let n = read(head args)
          putStr(show (fat n))
