-- Este código está protegido pela lei dos homens e pela leis de Deus, pq néh
--só ele sabe como essa desgraça funciona

{-# LANGUAGE FlexibleContexts #-}
-- Feito por Marlon Henry Schweigert

import Text.ParserCombinators.Parsec
import Data.Char

data Exp = Value Bool | And Exp Exp | Or Exp Exp | Not Exp | Imp Exp Exp | Bimp Exp Exp | Par Exp | Var String deriving Show

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

main = do {putStr "\nExpressao:";
          e <- getLine;
		  case expToTree e of
			Left err -> putStr ((show err)++ "\n")
			Right r  -> putStr ((show r) ++ "\n")}

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

