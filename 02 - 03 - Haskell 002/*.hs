inverte [] = []
inverte (x:xs) = (inverte xs)++[x]

palindromo txt1 txt2 = txt1==inverte(txt2)

existe el [] = False
existe el (x:xs) = x==el || existe el xs

intersesao [] a = []
intersesao a [] = []
intersesao [] [] = []
intersesao (x:xs) ys = (if existe x ys then x:intersesao xs ys else intersesao xs ys)

repetidos [] = []
repetidos x:xs = (if existe x xs then x:repetidos xs else repetidos xs)

contar [] = 0
contar (x:xs) = 1+contar(xs)

menor [] = []
menor (x:xs) = if x < menor(xs) then x else menor(xs)