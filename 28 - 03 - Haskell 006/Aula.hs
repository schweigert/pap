-- 001 --

listaUns = 1:listaUns

-- take 100 listaUns --

-- 002 --

listaCrescente a = a:listaCrescente (a+1)

-- 003 --

listaCrescenteN a b = a:listaCrescenteN(a+b) b

-- 004 --

-- toEnum::(Enum a) => int-> a
-- fromEnum::(Enum a) => a->int

-- map fromEnum "ABCDEF"

-- map aplica uma função a todos os elementos
-- take pega os primeiros elementos da lista

enumerar a = a:enumerar(toEnum(fromEnum a+1))

---- COMPREENSSÃO DE LISTAS ----

-- {x+2 | x E N}
-- {x | x E N e X mod2==0}

-- [X*2 | x <-[1..100]]

-- [(x*2,y+2) | x<-[1..100], y<-[1..2]]

-- [x | x<-[1..] , x mod 2 = 0 , x mod 3 = 0]

multiplos a b = ([x| x<-[1..], mod x a == 0 , mod x b == 0]) -- 30 primeiros multiplos de 2 e 3

maiusculas s = [x | x <- s, elem x ['A'..'Z']]
