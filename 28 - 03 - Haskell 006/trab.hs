-- 001 --
uniao (s) [] = s
uniao (s) (y:ys) = y:uniao s ys

-- 002 --

existe el [] = False
existe el (x:xs) = el==x || existe el xs


intercessao [] [] = []
intercessao [] s = []
intercessao s [] = []
intercessao s (x:xs) = if existe x s then x:intercessao s xs else intercessao s xs

unico [] = []
unico (x:xs) = if existe x xs then unico xs else x:unico xs

-- 003 --

tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

ultimos n [] = []
ultimos 0 s = []
ultimos n (x:xs) = if tamanho xs <= n then xs else ultimos n xs

-- 004 --

expo x 0 = 1
expo x y = if y <= 1 then x else x*expo x (y-1)

-- 005 --

    -- _  _ _ _ _
    -- 16 8 4 2 1

bint [] = 0
bint (x:xs) = if x == '1' then expo 2 (tamanho xs) + bint xs else bint xs

-- 006 --

existemenor el [] = False
existemenor el (x:xs) = el > x || existemenor el xs

menor [x] = x
menor (x:xs) = if existemenor x xs then menor xs else x

-- 007 --

remove el [] = []
remove el (x:xs) = if el == x then remove el xs else x: (remove el xs)

sorteia [] = []
sorteia [x] = [x]
sorteia s = menor(s):(sorteia (remove (menor s) s))

-- 008 --

insere el (x:xs) = sorteia ([el]++[x]++xs)

-- 009 --

par 0 = True
par 1 = False
par el = if mod el 2 == 1 then False else True


-- 010 --

analisa f [] = []
analisa f (x:xs) = if f(x) then x:analisa f xs else analisa f xs

-- 011 --



impares [] = []
impares s = removeli (analisa par s) s
	where
		removeel e [] = []
		removeel e (x:xs) = if e == x then removeel e xs else x:removeel e xs
		removeli [] s = s
		removeli (x:xs) s = removeli xs (removeel x s)

-- 012 --

mapl _ [] = []
mapl f (x:xs) = f x:mapl f xs

primeiroDupla s = mapl primeiro s
	where
		primeiro (x,_) = x
		
-- 013 --

folda _ a [] = a
folda f a (x:xs) = folda f (f a x)xs 

verList (x:xs) = folda func x xs
	where
		func a b = a || b
		
-- 14 --

sumList (x:xs) = folda func x xs
	where
		func a b = a+b
		
-- 15 --

listaToDupla [] a = []
listaToDupla a [] = []
listaToDupla (x:xs) (y:ys) = makeDupla x y:listaToDupla xs ys
	where
		makeDupla a b = (a,b)

-- 16 --


