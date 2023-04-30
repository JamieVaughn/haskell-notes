-- quick sort
qs :: (Ord a) => [a] -> [a]  -- types begin with upper case (Ord)
qs [] = []
qs (x:xs) = qs ys ++ [x] ++ f zs 
           where -- layout rule: whitespace is significant
              ys = [a | a <- xs, a <= x]
              zs = [b | b <- xs, b > x]

xs -- by convention list args have an "s" suffix
nss -- list of lists

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

-- function & arg identifiers must start with lowercase
f' a = a * a
-- function application denoted with spaces
f a b + c * d -- f(a,b) + c * d 

-- PFEMDAS, not PEMDAS... Parens & function application is first
f a + b -- f(a) + b
f (g x) -- f(g(x))
f x (g y) -- f(x, g(y))
f x * g y -- f(x) * g(y)

-- infix function
add x y = x + y
x `add` y -- backticks make add infix

-- Booleans aka Bool
not True || False -- capital letter identifiers are special data constructors

-- Strings aka [Char]
"Hello" ++ " World"
length "Hello" -- can also use head, tail, last, init, reverse, null builtin functions on Strings
'H' -- single Char uses single quotes
"Hello" -- list of Chars uses double quotes (aka String)

-- Errors
-- static errors happen at compile time 
-- dynamic errors happen at runtime -- head ""
-- lexical error is a static error regarding syntax -- "Hello
-- type error indicate a semantic fault like providing the wrong type to a function -- not "Hello"

-- Types, uses [] and leaves length unrestricted
:t True -- finds the type True :: Bool, "Hi" :: [Char] <- list of Chars
:t not -- not :: Bool -> Bool
:t length -- [a] -> Int -- a is a generic type
[True, "hello"] -- type error, lists must be of the same type

-- Tuple, uses () and specifies length
(False, True) :: (Bool, Bool) -- Tuple defines type and lengths

-- Guarded Equations -- as an alternative to `if then else` ternary expressions
-- the catchall condition otherwise is defined in the prelude by otherwise = True
signum n | n < 0     = -1
         | n == 0    = 0
         | otherwise = 1

-- Pattern Matching
not       :: Bool -> Bool
not False = True
not True  = False

(&&)          :: Bool -> Bool -> Bool
True && True   = True
True && False  = False
False && True  = False
False && False = False
-- which can be expressed more concisely with point free variable syntax using `_`
True && True = True
_    && _    = False
-- which can also be expressed another way with piped variable of b (cannot repeat b, though)
True  && b = b
False && _ = False

-- examples -- parens are needed because function application binds strongest)
head      :: [a] -> a
head (x:_) = x

tail       :: [a] -> [a]
tail (_:xs) = xs
-- these both will error on empty list because no case will match

-- Sections -- partially applying functions
(+) 1 2 -- 3
(1+) 2 -- 3
(+2) 1 -- 3

-- List Comprehensions
[x^2 | x <- [1..5]] -- creates list of squares from 1 through 5
[(x,y) | x <- [1,2,3], y <- [4,5]] -- creates [(1,4),(1,5),(2,4),(2,5),(3,4),3,5)] similar to nested loops. y is inner loop, x is outer loop
-- change order of generators
[(x,y) | y <- [4,5], x <- [1,2,3]]
-- Dependent Generators -- similar to inner loop and outer loop, where inner loop can use variables in the outerloop
[(x,y) | x <- [1..3], y <- [x..3]] -- = [(1,1),(1,2),(1,3),(2,2),(2,3),(3,3)], all pairs of nums (x,y) with x,y as elements of [1..3] and y >= x
concat    :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]
concat [[1,2,3],[4,5],[6]] -- = [1,2,3,4,5,6]
-- Filters, Guards
[x | x <- [1..10], even x] -- eve x "where clause" is called a guard
factors  :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- use factors to find a prime number
prime  :: Int -> Bool
prime n = factors n == [1,n]
-- use prime to find a list of primes
primes  :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]
primes 40 -- generates primes up to 40

-- zip function
zip :: [a] -> [b] -> [(a,b)]
zip ['a','b','c'] [1,2,3,4] -- [('a',1),('b',2),('c',3)] (omits the 4 because it stops at the shortest length input)

pairs   :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)
pairs [1,2,3,4] -- [(1,2),(2,3),(3,4)]

sorted.  :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs] -- checks if a list is already sorted
