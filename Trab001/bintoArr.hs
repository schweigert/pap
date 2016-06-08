tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

bintoArr [] = []
bintoArr a = (translate (take 8 a)) : bintoArr (remove 8 a)
   where
      remove _ [] = []
      remove 0 s = s
      remove n (x:xs) = remove (n-1) xs
      translate (x:[]) = if x == '1' then 1
                                     else 0
      translate a@(x:xs) = if x == '1' then 2^(length xs) + translate xs
                                       else translate xs


