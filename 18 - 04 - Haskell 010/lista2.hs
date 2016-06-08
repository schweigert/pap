import Data.List

-- main --

toUpper xs = [ if x < 'a' then x else maybe x id $ lookup x charMap
         | x <- xs, (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') ]
  where charMap = zip ['a' .. 'z'] ['A' .. 'Z']

size [] = 0
size (x:xs) = 1 + size xs



type Doc = String
type Line = String
type Word = String

texto = "Lorem ipsum dolor sit amet\n consectetur adipiscing elit Fusce justo sem scelerisque\neget ligula eget egestas molestie odio. Proin ut sagittis nibh\n ac rhoncus turpis. Class aptent taciti sociosqu ad litora torquent per conubia nostra\n per inceptos himenaeos\n Donec vel euismod nibh. Aliquam erat volutpat\n Sed sagittis tempor scelerisque Cras convallis velit vel risus pretium a gravida augue posuere\n Maecenas ut placerat tellus\n Proin eleifend malesuada tristique\n In malesuada consectetur arcu at lacinia"

makeIndex = sortLs(allNumWords(numLines(separarL texto)))

-- 001 --
separarL txt = lines txt

-- 002 --

numLines ls = zip [1..size ls] ls

-- 003 --
allNumWords [] = []
allNumWords (x:xs) = (apply x):(allNumWords xs) 
	where
		apply (linha, txt) = zip (makeList linha) (words txt)
		makeList n = n:makeList n

-- 004 --

sortLs [] = []
sortLs (x:xs) = (sortBy (comp) x) ++ sortLs xs
	where
		comp (_,x) (_,y)
			| toUpper(x) > toUpper(y) = GT
			| toUpper(x) < toUpper(y) = LT
			| otherwise = EQ
		
-- 005 --

alma [] = []
alma (x:xs) = (apply x xs) : alma xs
	where
		apply (l,w) s = (getLinhas w s, w)
		getLinhas _ [] = []
		getLinhas p (x:xs) = if toUpper(pegaW x) == toUpper p then pegaL x ++ (getLinhas p xs) else (getLinhas p xs)
		pegaL (l,_) = l
		pegaW (_,w) = w
		
