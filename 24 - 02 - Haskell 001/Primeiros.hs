primeiros (x:xs) 0 = []
primeiros [] n = []
primeiros (x:xs) n = x : primeiros xs (n-1)

concatena [] ys = ys
concatena (x:xs) ys =  x:concatena xs ys

