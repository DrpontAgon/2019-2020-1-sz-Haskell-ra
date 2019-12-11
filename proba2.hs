{-#LANGUAGE LambdaCase#-}

import Data.Char

{-f :: (Int, Int) -> Int
f (a,_) = 3 * a + 2-}

g :: Bool -> Bool
g _ = True

h [] = True
h _ = False

h' [y] = y
h' [x,y] = x
h' _ = 9

h'' (x:xs) = x
h'' _ = 10
-- Segítség: fromEnum False == 0; fromEnum True == 1
-- f (length [0,2..10], last [])
-- f (fromEnum (g ((4+5) `div` (9-9) == 9)), f(head [], length [1..]))

sl :: Integer -> Bool -> [Integer] -> Integer
sl 1 False (3:[]) = 4
sl 0 False (xs:x) = xs
sl _ True (y:ys) = y
sl x False _ = x
sl 1 True (x:xs) = 1 + x
sl _ _ _ = 0
sl _ True [] = 100

-- sl 0 (not True) [9..]
-- sl 1 (sl 2 (null []) [] == 100) [1+2]

a :: Int -> Int
a x = 6 + x - x

b :: Double -> Double
b x = 1.0

c :: Int -> Double -> Double
c x y = y + 3

-- a 9
-- a (head [])
-- b (last [])
-- c (length [10..]) (b (head []))
-- b (a (length [1..]))


-- Egyik konzultáción coding versenyeztünk, hogy ki tudja a legrövidebb kódot írni, ami ellenőrzi, hogy egy tetszőleges szövegben helyes-e a zárójelezés.
-- Én nyertem! :)
-- Gyorsaságra nem, mert én csak bő félidő után csatlakoztam be.
goodP :: String -> Bool
goodP a=let s=(\case('(':')':x)->x;('[':']':x)->x;('{':'}':x)->x;a->a)in null$until((==)=<<s)s(filter(`elem`"()[]{}")a)
-- a ( goodP a= ) utáni rész számított bele csak a karakterszámba.

f :: [(Int,Int)] -> Int
f ((x1,x2):(y1,y2):_) = x1+x2+y1+y2

timeAdd :: (Int,Int) -> (Int,Int) -> (Int,Int)
timeAdd (h1,m1) (h2,m2) = ((h1+h2+(m1+m2) `div` 60) `mod` 24,(m1+m2) `mod` 60)

isSmile :: String -> Bool
isSmile [x,y] = x `elem` ":;" && y `elem` ")}]"
isSmile _     = False

bimBam :: Int -> String
bimBam x
    | x `mod` 15 == 0 = "BimBam"
    | x `mod` 3 == 0  = "Bim"
    | x `mod` 5 == 0  = "Bam"
    | otherwise       = ""

minList :: [Int] -> [Int] -> [Int]
minList (x:xs) (y:ys) = min x y : minList xs ys
minList _      _      = []

minList' xs ys = [min x y | (x,y) <- zip xs ys]

wordNumWithCapital :: String -> Int
wordNumWithCapital str = sum [1 | (x:_) <- words str, isUpper x]

oneMatrix :: Int -> [[Int]]
oneMatrix n = replicate n (replicate n 1)

func :: [a] -> [a]
func (xs:x:y) = xs : x : x : func y
func list     = list

lucas 0 = 2
lucas 1 = 1
lucas n | n >= 2 = lucas' n (2,1)
 where
  lucas' 1 (_,a) = a
  lucas' n (a,b) = lucas' (n-1) (b, a+b)
  
newtype EvenInteger = EvenInteger Integer deriving (Eq,Ord)

instance Enum EvenInteger where
    succ (EvenInteger integer) = EvenInteger (succ $ succ integer)
    pred = fromInteger . pred . pred . toInteger
    toEnum = fromInteger . toEnum
    fromEnum = fromEnum . toInteger
    enumFrom (EvenInteger start) = map EvenInteger (enumFrom start)
    enumFromThen (EvenInteger start) (EvenInteger next) = map EvenInteger $ enumFromThen start next
    enumFromTo (EvenInteger start) (EvenInteger end) = map EvenInteger (enumFromThenTo start (succ $ succ start) end)
    enumFromThenTo (EvenInteger start) (EvenInteger next) (EvenInteger end) = map EvenInteger $ enumFromThenTo start next end

instance Show EvenInteger where
    showsPrec prec (EvenInteger integer) = showsPrec prec integer

instance Read EvenInteger where
    readsPrec prec str = map (first fromInteger) (readsPrec prec str) where
        -- This is Control.Arrow.first, specialized to (->).
        first :: (val -> val') -> (val,other) -> (val',other)
        first fun (val,other) = (fun val,other)

instance Num EvenInteger where
    EvenInteger integer1 + EvenInteger integer2 = EvenInteger (integer1 + integer2)
    EvenInteger integer1 - EvenInteger integer2 = EvenInteger (integer1 - integer2)
    EvenInteger integer1 * EvenInteger integer2 = EvenInteger (integer1 * integer2)
    abs (EvenInteger integer) = EvenInteger (abs integer)
    signum (EvenInteger integer) = EvenInteger (signum integer)
    fromInteger integer | even integer = EvenInteger integer
                        | otherwise    = error "EvenInteger: EvenInteger cannot be odd!"

instance Real EvenInteger where
    toRational = toRational . toInteger

instance Integral EvenInteger where
    quotRem (EvenInteger integer1) (EvenInteger integer2) = let (quot,rem) = quotRem integer1 integer2 in if odd quot then (EvenInteger (quot-1),EvenInteger (rem+integer2)) else (EvenInteger quot,EvenInteger rem)
    divMod = quotRem
    toInteger (EvenInteger integer) = integer
    
positiveProduct :: [Int] -> Int
positiveProduct l = product [n | n <- l, n > 0]

mightyGale :: [(String,Int,Int,Int)] -> String
mightyGale [] = []
mightyGale ((a,_,c,_):xs)
    | c >= 110 = a
    | otherwise = mightyGale xs

cipher :: String -> String
cipher (x:y:z:xs)
    | isLetter x && isLetter y && isDigit z = [x,y]
    | otherwise = cipher (y:z:xs)
cipher _ = []

pizza :: [(String, Int)] -> Int
pizza l = 500 + sum [b | (_,b) <- l]

listDiff :: String -> String -> String
listDiff x y = [c | c <- x, c `notElem` y]

validGame :: String -> Bool
validGame [] = True
validGame l = validGame' (words l)
    where
        validGame' [] = True
        validGame' [_] = True
        validGame' (x:y:xs) = last x == head y && validGame' (y:xs)

prodAllButMod5Odd :: [Int] -> Int
prodAllButMod5Odd l = product [n | n <- l, let b = n `mod` 5, even b]

and3 x y z = x && y && z

and' :: [Bool] -> Bool
and' (x:xs) = x && and' xs
and' []     = True

{-fib'' 0 = 1
fib'' 1 = 1
fib'' n = fib'' (n-1) + fib'' (n-2)-}

fib :: Integer -> Integer
fib n | n < 0 = 0
fib 0 = 1
fib 1 = 1
fib n = fib' n (1,1)
    where 
        fib' 1 (_,b) = b
        fib' n (a,b) = fib' (n-1) (b,a+b)

unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' ((a,b):xs) = (a : as, b : bs)
    where
        (as,bs) = unzip' xs

nub' :: Eq a => [a] -> [a]
nub' list = nub'' list []
    where
        nub'' []     _       = []
        nub'' (x:xs) visited
            | x `elem` visited = nub'' xs visited
            | otherwise = x : nub'' xs (x:visited)

nubAlt :: Eq a => [a] -> [a]
nubAlt []     = []
nubAlt (x:xs) = x : nubAlt [y | y <- xs, y /= x]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' x _ | x <= 0 = []
take' x (y:ys) = y : take' (x-1) ys

drop' :: Int -> [a] -> [a]
drop' _ []          = []
drop' x ys | x <= 0 = ys
drop' x (y:ys)      = drop' (x-1) ys

geometricSequence :: Integer -> Integer -> [Integer]
geometricSequence s q = s : geometricSequence (s*q) q

removeDuplicateSpaces :: String -> String
removeDuplicateSpaces (' ':' ':xs) = removeDuplicateSpaces (' ':xs)
removeDuplicateSpaces (x:y:xs)     = x : removeDuplicateSpaces (y:xs)
removeDuplicateSpaces list         = list

splitOn :: Eq a=>a->[a]->[[a]]
splitOn _ [] = []
splitOn d list 
 | maradek == [d] = alist : [] : splitOn d (drop 1 maradek)
 | otherwise = alist : splitOn d (drop 1 maradek)
   where 
    (alist,maradek) = span (/=d) list

decompress :: [(a, Int)] -> [a]
decompress list = concatMap (uncurry $ flip replicate) list

data IPoint = P Int Int | P' Integer Integer | P'' Float Float

instance Show IPoint where
    show (P x y) = '(':show x ++ ',':show y ++ ")"
    show (P' x y) = '(':show x ++ ',':show y ++ ")"
    show (P'' x y) = '(':show x ++ ',':show y ++ ")"






