-- quick sort
qs :: (Ord a) => [a] -> [a]  
qs [] = []
qs (x:xs) = qs ys ++ [x] ++ f zs
           where
              ys = [a | a <- xs, a <= x]
              zs = [b | b <- xs, b > x]
              
head [1,2,4,5,6,]  -- 1
tail [1,2,3,4,5] -- 5
[1,2,3,4,5] !! 2 -- 3
take 3 [1,2,3,4,5] -- [1,2,3]
drop 3 [1,2,3,4,5] -- [4,5]
length [1,2] --2
sum [1,2,3] -- 6
product [1,2,3]
[1] ++ [2] -- [1, 2]
reverse [0,1] -- [1,0]

-- function application denoted with spaces
f a b + c * d -- f(a,b) + c * d 

-- PFEMDAS, not PEMDAS... Parens & function application is first
f a + b -- f(a) + b
f (g x) -- f(g(x))
f x (g y) -- f(x, g(y))
f x * g y -- f(x) * g(y)
