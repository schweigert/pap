{-
Arquivos em Binario
-}

import qualified Data.Binary.Put as P
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I

--qualified "renomeia" as f do Data para P.nameF

escrita = do
            txt <- readFile "texto.txt"
            let xs = freq txt
            let bs = P.runPut(put (xs))
            L.writeFile "teste.bin" bs

freq [] = []
freq (x:xs) = (x,length(filter(==x) xs)+1):freq(filter (/=x) xs)

--Funcao put coloca os dados num monado put, cria um dado do tipo Put
--      putWord transforma como um bite 8bits
put [] = P.flush
put ((c,f):xs) = do
                    P.putWord8 (I.c2w c)
                    P.putWord32be (toEnum f)
                    put xs