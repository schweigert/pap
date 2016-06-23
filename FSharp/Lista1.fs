// Para abrir o compilador: fsharpc
// Interpretador: fsharpi

// Dados para teste

let lista1 = [1..7]
let lista2 = [3..10]

lista3
// -- 001 -- //
// Retornar a união de duas Lista

let uniao a b = a @ b

// -- 002 -- //
// Retornar a intersessão entre duas listas

let rec intersessao a b = 
    match a with
    | x::xs -> match b with
               | y::ys ->
                    if x==y then x::(intersessao xs ys)
                    else if xs > ys then intersessao xs b else intersessao a ys
               | [] -> []
    | [] -> []

// -- 003 -- //
// Retornar os n ultimos elementos de uma lista

let rec tamanho lista =
    match lista with
    | x::xs -> 1 + tamanho xs
    | [] -> 0

let rec ultimos lista n =
    let rec remove a =
        match a with
        | x :: xs -> xs
        | [] -> []
    if tamanho lista > n then ultimos (remove lista) n else lista

// -- 004 -- //



let rec binToInt texto =
    let rec power a b =
        match b with
        | 0 -> 1
        | 1 -> a
        | n -> a*power a (b-1)
        

binToInt "1"

