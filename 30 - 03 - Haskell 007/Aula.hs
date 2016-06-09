fib 0 = 0
fib 1 = 1
fib n = fib(n-2) + fib(n-1)

fibs = map fib [0..]

zip [] _ = []
zip + [] = []
zip (x:xs) (y:ys) = (x,y):zip xs ys

zipW _ _ [] = []
zipW _ [] _ = []
zipW f (x:xs) (y:ys) = f x y : ZipW f xs ys

fibs' = 0:(1:2 zipW (+) fibs' (tail fibs')