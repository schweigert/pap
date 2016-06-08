
{-

	-- tipos de dados algébricos --

	   é um tipo formado pela combinação de tipos. O termo algébrico é
	usado porque os tipos são constituidos usando operações de soma e produto
	
	Soma (Alternação): data Bool = True|False deriving Show
	Produto (Combinação): data Par a b = Dupla a b deriving Show
	
	Enumeração:
	
	data Semana = Domingo | Segunda | Terça | Quarta | Quinta | Sexta | Sábado
	
	Produto:
		
	data Agenda = Contato String String deriving Show
	
	Polimórfico:
	
	data Par a b = Dupla a b deriving Show
	
	Recursivo:
	
	data ListaInt = Vazio | Cons ListaInt deriving Show
	
-}



data Semana = Dom | Ter | Qua | Qui | Sex | Sab deriving Show
data Agenda = Contato String String deriving Show

telefone (Contato n t) = t

data Par a b = Dupla a b deriving Show

primeiro (Dupla a b) = a
segundo (Dupla a b) = b

data ListaInt = Vazio | Cons Int ListaInt deriving Show

len Vazio = 0
len (Cons x xs) = 1+len xs

------------------------------------------------------------------------
data Arvore a = No a (Arvore a) (Arvore a) | Folha deriving Show

insere e Folha = No e Folha Folha
insere e a@(No x esq dir)
 	| e < x = No x (insere e esq) dir
 	| e > x = No x esq (insere e dir)
	| otherwise = a


contar Folha = 0
contar (No e Folha x) = 1 + contar x
contar (No e x Folha) = 1 + contar x
contar (No e x y) = 2 + (contar x) + (contar y)

insereEm f e Folha = No e Folha Folha
insereEm f e a@(No x esq dir)
	|
	|
	where
		elemento Folha = 
		elemento (No x _ _) = just x
------------------------------------------------------------------------


