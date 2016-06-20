// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module Fibonacci =
    let rec fib n =
        match n with
        | 1 | 2 -> 1
        | n -> fib(n-1) + fib(n-2)

[<EntryPoint>]
let main argv = 
    printfn "Primeiro código em FSharp"
    printfn "Sequencia de Fib"
    let f = Fibonacci.fib 30
    printfn "%d" f

    0 // return an integer exit code
