-- Tipos

    -- Bool
    -- Int
    -- Integer
    -- Float
    -- Double
    -- [Int]
    -- [Char] String
    -- [[Char]] [String]
    -- (1,'a') :: (Int, Char)
    -- (10, "Teste") :: (Int, [Char])

-- Validação de Expressão
    -- > length [0,1,2,3,4]
    -- > 5

-- funcoes

    -- dobro x = x + x
    
    -- maior a b = if a > b then a else b
    
    -- fat 0 = 1
    -- fat n = n*fat(n-1)
    
    -- len (x:xs) = 1 + len xs
    -- len [] = 0
        -- len[1,2,3]
            --> 1+len[2,3]
            --> 1+1+len[3]
            --> 1+1+1+len[]
            --> 1+1+1+0
            --> 3

-- Contrutor de Lista
    -- a:b
    -- 1:2
        --> [1,2,3]
    -- 1:[2,3]
    -- 2:[3]
    -- 3:[]

