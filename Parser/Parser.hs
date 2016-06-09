import Text.ParserCombinators.Parsec
import Data.Char

data Exp = Value Bool | And Exp Exp | Or Exp Exp | Not Exp | Imp Exp | Bimp Exp | Par Exp | Var String

{-

	E -> E+E
	   | E*E
	   | E->E
	   | E<->E
	   | !E
	   | (E)
	   | V
	   
	V -> T
	   | F

   

    ------------------------------

    E -> E<->F
        | F

    F -> F -> S
        | S

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

    W: C
     | (E)
-}

ret v1 Nothing = v1
ret v1 (Just (op, v2)) = op v1 v2

expToTree e = parse exp "Erro:" e

{-
exp = do {
    v1 <- termo
    o <- op
    return ( ret v1 o)
}

exp' = do {
            char '&';
            v1 <- termo;
            e <- exp';
            return (Just ((And), ret v1 e))
        }
        <|> do {
            char '|';
            v1 <- termo;
            e <- exp';
            return (Just((Or), ret v1 e))
        }
        <|> return Nothing

termo = do {
    v1 <- fator
    e <- termo'
    return (ret v1 e)
}

termo' = do {
            string "<->";
            v1 <- fator;
            e <- termo';
            return (Just (Bimp, ret v1 e))
        }
        <|> do {
            string "->";
            v1 <- fator;
            e <- termo';
            return (Just (Imp, ret v1 e))
        }
        <|> return Nothing

fator = terminal 
        <|> do {char '(';
                e <- exp; char ')';
                return (Par e)
               }

terminal = negacao <|> bool <|> var

negacao = do {char '!'; e <- exp; return (Not e)}

var = do { return Noting }

bool = do { char '1'; return Value True }
      <|> { char '0'; return Value False }
-}
