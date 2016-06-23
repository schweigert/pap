type Btree<'a>=
    | No of Btree<'a> * Btree<'a>
    | Folha of 'a

let rec inserir(arvore valor) =
    match arvore with
        |   No a b ->
        |   Folha a -> 