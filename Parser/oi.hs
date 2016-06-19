{-# LANGUAGE FlexibleContexts #-}
-- Este código está protegido pela lei dos homens e pela leis de Deus, pq néh
--só ele sabe como essa desgraça funciona

--


import Text.ParserCombinators.Parsec
import Data.Char

data Exp = Value Bool | And Exp Exp | Or Exp Exp | Not Exp | Imp Exp Exp | Bimp Exp Exp | Var String deriving Show

{-

    ------------------------------

    E -> E<->F
        | F

    F -> F -> S
        | S,

    S -> S | R
        | R

    R -> T & N
        | V

    V -> !V
        | W

    W -> (E)
        | C

    ------------------------------

    Tirando a recursão

     E: FE'
    
    E': <-> FE'
      | Vazio

     F: SF'

    F':  ->SF'
      | Vazio

     S: RS'
    
    S': |RS'
      | Vazio

     R: VR'

    R': &VR'
      | Vazio

     V: !V
      | W

    W: c
     | (E)

    E - inicio
    c - variaveis
-}


ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2

bin m n = (take (m - length (toBin n)) ['0','0'..]) ++ reverse (toBin n)
	where
		toBin 1 = "1"
		toBin 0 = "0"
		toBin x = if mod x 2 == 1 then "1" ++ (toBin (div x 2))
								  else "0" ++ (toBin (div x 2)) 
atribui [] = []
atribui a = montar a [0..]
	where
		montar [] _ = []
		montar (x:xs) (y:ys) = (x,y): montar xs ys



resolverParanaue a [] duplas = []
resolverParanaue a (x:xs) duplas = solve(arvore):resolverParanaue a xs duplas
        where
                arvore = mapeamentoLogico a x duplas
                


        

main = do {putStr "\nExpressao:";
          e <- getLine;
		  case expToTree ([x | x <- e, x /= ' ']) of
			Left err -> putStr ((show err)++ "\n")
			Right arv  -> do 
				
				putStr ("Arvore criada: \n")
				putStr ((show arv) ++ "\n")
				putStr "Variáveis: \n"
				let variaveis = atribui(mapear(arv))
--				    binli = map (bin (length variaveis)) [0..(length variaveis)^2 - 1]
				    binli = map (bin (length variaveis)) [0..(2 ^ length variaveis ) - 1]
				    possibilidade1 = mapeamentoLogico arv (binli!!0) variaveis
				putStr (show (variaveis)++"\n")
				putStr ("Possibilidades:\n")
				putStr (show (binli)++"\n")
				putStr ("Testando Possibilidades...\n")
				let resposta = resolverParanaue arv binli variaveis
				putStr (imprimirResp binli resposta resposta)

			}

verificarResp [] = False
verificarResp (x:xs) = x || verificarResp xs

imprimirResp [] _ a = "Possui Solucao? \t"++(show (verificarResp a))++("\n")
imprimirResp (x:xs) (y:ys) a = (show x )++ " | " ++(show y)++ "\n" ++ (imprimirResp xs ys a)
        

expToTree e = parse elang "Erro:" e
	

-- E: FE'
elang = do {
        f <- flang;
        e' <- elang';
        return (ret f e')
    }

    
--    E': <->FE'
--      | Vazio

elang' = do {
        string "<->";
        f <- flang;
        e' <- elang';
        return (Just((Bimp),ret f e'))
    }
    <|> return Nothing



--     F: SF'

flang = do {
            s <- slang;
            f' <- flang';
            return (ret s f')
        }

--    F':  ->SF'
--      | Vazio

flang' = do {
            string "->";
            s <- slang;
            f' <- flang';
            return (Just((Imp), ret s f') ) 
        }
        <|> return Nothing

--     S: RS'

slang = do {
            r <- rlang;
            s' <- slang';
            return (ret r s')
        }
    
--    S': |RS'
--      | Vazio

slang' = do {
            char '|';
            r <- rlang;
            s' <- slang';
            return (Just((Or), ret r s'))
        }
        <|> return Nothing

--     R: VR'

rlang = do {
            v <- vlang;
            r' <- rlang';
            return (ret v r');
        }

--    R': &VR'
--      | Vazio

rlang' = do {
            char '&';
            v <- vlang;
            r' <- rlang';
            return (Just((And), ret v r'))
        }
        <|> return Nothing

--     V: !V
--      | W

vlang = do {
            char '!';
            v <- vlang;
            return (Not v)
        }
        <|> do {
                w <- wlang;
                return (w)
            }

--    W: c
--     | (E)

wlang = do {
            char '1';
            return (Value True)
        }
        <|> do {
            char '0';
            return (Value False)
        }
        <|> do {
            letra <- letter; 
            variable <- many (digit <|> letter);
            return (Var ([letra]++variable))	
        }
        <|> do {
            char '(';
            e <- elang;
            char ')';
            return e;
        }

--    E - inicio
--    c - variaveis

--list do marlons e eu

solve (Value a) = a
solve (And a b) = (solve a) && (solve b)
solve (Or a b) = (solve a) || (solve b)
solve (Imp a b) = imp (solve a) (solve b)
	where
		imp False False = True
		imp False True = True
		imp True False = False
		imp True True = True
solve (Bimp a b) = bimp (solve a) (solve b)
	where
		bimp False False = True
		bimp False True = False
		bimp True False = False
		bimp True True = True	
solve (Not a) = if solve a then False else True
solve (Var a) = True

mapear e = removeDuplicates mapa
	where
		mapear_ (Value _) = []
		mapear_ (And a b) = mapear_ a ++ mapear_ b
		mapear_ (Or a b) = mapear_ a ++ mapear_ b
		mapear_ (Imp a b) = mapear_ a ++ mapear_ b
		mapear_ (Bimp a b) = mapear_ a ++ mapear_ b
		mapear_ (Not a) = mapear_ a
		mapear_ (Var a) = [a]
		mapa = mapear_ e
		removeDuplicates = foldl (\seen x -> if x `elem` seen
                                      then seen
                                      else seen ++ [x]) []

--mudar para 1 e 0
mapeamentoLogico a bin duplas = resolver a
	where
		resolver (Var a) = if bin!!(procurar duplas a) == '1' then Value True else Value False
		resolver (And a b) = And (resolver a) (resolver b)
		resolver (Or a b) = Or (resolver a) (resolver b)
		resolver (Imp a b) = Imp (resolver a) (resolver b)
		resolver (Bimp a b) = Bimp (resolver a) (resolver b)
		resolver (Not a) = Not (resolver a)
 		procurar (x:xs) e = if fst x == e then snd x else procurar xs e
 		

