// FSharp <3
// By Marlon Henry

module Fibonacci =
    let rec fib n =
        match n with
        | 1 | 2 -> 1
        | n -> fib(n-1) + fib(n-2)

module Primos =
    let Teste n x =
        x = n || x % n <> 0
    let rec RemoverTodosMultiplos listn listx =
        match listn with
        | head :: tail -> RemoverTodosMultiplos tail (List.filter (Teste head) listx)
        | [] -> listx

module Lista =
    let rec sum list =
        match list with
        | head :: tail -> head + sum tail
        | [] -> 0
    let rec tam list =
        match list with
        | head :: tail -> 1 + tam tail
        | [] -> 0

// Tipagem

type Livro = {titulo: string; preço: decimal}

type TipoDeChocolate = Nescau | Toddy | OutrosPiores
type Chocolate = {chocType: TipoDeChocolate ; preço: decimal}

type TipoDePapel = 
    | PapelDeAniversario
    | PapelDeNatal
    | CorSolida

type Presente =
    | Book of Livro
    | Chocolate of Chocolate 
    | Envio of Presente * TipoDePapel
    | Encaixotado of Presente 
    | ComCartao of Presente * mensagem:string

[<EntryPoint>]
let main argv = 
    printfn "Primeiro código em FSharp"
    // Listas
    let lista = [1 .. 50]
    printfn "Listas:"
    printfn "Somatório [1..50] = %d" (Lista.sum lista)
    printfn "Tamanho [1..50] = %d" (Lista.tam lista)
    // Recursividade
    printfn "Sequencia de Fib"
    let fatorFib = Fibonacci.fib 16
    printfn "%d" fatorFib
    0 // return an integer exit code
