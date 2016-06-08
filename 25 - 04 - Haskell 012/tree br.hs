 {-
 
	1. Todo nó é vermelho ou preto
	2. Toda folha é preta
	3. Se um nó é vermelho seus filhos são pretos
	4. Umnovo nó é inserido como vermelho
	5. A raiz é preta
	
	pior caso: 2log(2,n+1) -: Distância entre as pontas da árvore
	
	3p
	| \
	1v d
	| \
	a  2v
	   | \
	   b  c

	3p
	|  \
	2v  d
	| \
	1v c
	|\
	a v
	
	1p
	| \
	a  2v
	   | \
	   b  3v
	      | \
	      c  d
	
	1p
	| \
	a  3v
	   | \
	   2v d
	   | \
	   b  c
	   
	IGUAL
	
	 2v
	|   \
	1p   3p
	| \  | \
	a  b c  d
 -}

data Cor = V|P deriving Show
data Arvore a = No a Cor (Arvore a) (Arvore a) | Folha deriving Show

rot (No x3 P (No x1 V a (No x2 V b c)) d) = No x2 V (No x1 P a b) (No x3 P c d)
rot (No x3 P (No x2 V (No x1 V a b) c) d) = No x2 V (No x1 P a b) (No x3 P c d)
rot (No x1 P a (No x2 V b (No x3 V c d))) = No x2 V (No x1 P a b) (No x3 P c d)
rot (No x1 P a (No x3 V (No x2 V b c) d)) = No x2 V (No x1 P a b) (No x3 P c d)
rot a = a

ins' e Folha = No e V Folha Folha
ins' e a@(No e1 c esq dir)
	| e < e1 = rot (No e1 c (ins' e esq) dir)
	| e > e1 = rot (No e1 c esq (ins' e dir))
	| True = a

ins e a = No e1 P esq dir
	where (No e1 c esq dir) = ins' e a

arv = foldr ins Folha (reverse [1..150])

cria n = foldr ins Folha (reverse n)
