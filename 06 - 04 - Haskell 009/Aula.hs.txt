-- 23 --

data Nat = Zero | Suc Nat deriving (Show, Eq)

toNat 0 = Zero
toNat e = Suc (toNat(e-1))

toNum Zero = 0
toNum (Suc a) = 1+ toNum a

maior a b = toNum a > toNum b
menor a b = toNum a < toNum b

soma Zero Zero = Zero
soma (Suc a) Zero = Suc a
soma Zero (Suc b) = Suc b
soma (Suc a) y = soma a (Suc y)

subt y Zero = y
subt (Suc a) (Suc Zero) = a
subt (Suc a) (Suc b) = subt (a) (b)

mult Zero Zero = Zero
mult Zero y = Zero
mult y Zero = Zero
mult (Suc a) (Suc b) = soma (Suc a) (mult (Suc a) (b))

expo a (Suc(Zero)) = a
expo (Suc a) Zero = toNat 1
expo Zero (Suc a) = Zero
expo y (Suc b) = mult y (expo y b)


