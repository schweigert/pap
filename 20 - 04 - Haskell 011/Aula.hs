import Data.List
import Data.Char


main2 = do putStr "Digite o Arquivo:\n"
           arq <- getLine
           txt <- readFile arq
           let a = makeindex txt
           putStr (mostrar a)


mostrar [] = ""
mostrar (x:xs) = show x ++ "\n" ++ mostrar xs

numLines = zip [1..]

remove [] = []
remove (x:xs) = if (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x == ' ' || x == '\n' then x: remove xs else remove xs

allNumWords [] = []
allNumWords((i,l):xs) = map (\w -> (i,w))(words l) ++ allNumWords xs

sortLs = sortBy cmpWord
	where
		cmpWord (_,w) (_,w') = compare (map toUpper w) (map toUpper w')
		
alma [] = []
alma xxs@(x:xs) = (map fst (takeWhile eqW xxs), snd x):alma (dropWhile eqW xs)
	where
		eqW (_,w') = map toUpper w' == map toUpper (snd x)
		
makeindex txt = alma(sortLs(allNumWords(numLines(lines(remove(txt))))))
