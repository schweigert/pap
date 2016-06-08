
data Semana = Dom | Seg | Ter | Qua | Qui | Sex | Sab
data Arvore a = No a (Arvore a)(Arvore a) | Folha

instance Show Semana where
    show Dom = "Domingo"
    show Seg = "Segunda"
    show Ter = "Terça"
    show Qua = "Quarta"
    show Qui = "Quinta"
    show Sex = "Sexta"
    show Sab = "Sabado"
    


instance (Show a) => Show(Arvore a) where
    show Folha = "Folha"
    show (No x esq dir) = "("++show esq ++") " ++ show x ++ " (" ++ show dir ++")"
    


class Len a where
	len::a -> Int


instance Len [a] where
	len [] = 0
	len (x:xs) = 1+len xs
	
instance Len (Arvore a) where
	
	len Folha = 0
	len (No _ esq dir) = 1 + len esq + len dir



class Map f where
	map'::(a->b)->f a->f b

instance Map Arvore where
	map' f Folha = Folha
	map' f (No a esq dir) = No (f a) (map' f esq) (map' f dir)

	
instance Map [] where
	map' = map

0