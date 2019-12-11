{-# LANGUAGE ViewPatterns, ParallelListComp #-}

--Mi fog történni a ghci-ben, ha az alábbi függvények vannak definiálva és az alábbi módon hívjuk meg azokat? A függvények mind helyesen vannak definiálva!
-- 5 válaszlehetőség:
-- konkrét eredményt kiszámolja és kiírja
-- ghci folyamatosan dolgozik és elkezd megjelenni valami eredmény
-- ghci folyamatosan dolgozik, de nem ír ki semmit
-- fordítási hiba (hosszú csúnya hibaüzenet, nem típushelyes kifejezés)
-- futási hiba (a sor, amit kiír, a ***Exception-nel fog kezdődni)
f :: Int -> Int
f x = 2 * x + 1

p :: Bool -> Bool
p c = False && c

h :: Integer -> Int -> Integer
h i j = i + 2

g :: Double -> Double
g x = 6.0

-- Az alábbiak kerülnek a ghci-be:
-- példa:
-- h 9 (f 5) -> 11-et fog kiírni.
---------------------------------
-- f (f 4)                             => 19
-- f (length [1..])                    => dolgozik, nem ír ki semmit
-- g (head []) == 5.0                  => False
-- p (f 2 `div` 0 == 0)                => False
-- g (f 3 ^ (-2))                      => Fordítási hiba
-- f (length [1,2,6,7]) == h (h 2 1) 3 => Fordítási hiba
-- h (last []) (length [0,2..50]) == 2 => Futási hiba
-- f (fromEnum (g (1/0) > 4))          => 3

{-sl :: Integer -> Bool -> [Integer] -> Integer
sl 1 False (3:[]) = 4
sl 0 False (xs:x) = xs
sl _ True (y:ys) = y
sl x False _ = x
sl 1 True (x:xs) = 1 + x
sl _ _ _ = 0
sl _ True [] = 100-}

-- sl 0 (not True) [9..]
-- sl 1 (sl 2 (null []) [] == 100) [1+2]

f' x
    | x < 0     = x + 1
    | otherwise = 2 * x
    
fact x 
    | x < 0 = 0
    | x == 0 = 1
    | otherwise = x * fact (x - 1)

fact' x 
    | x < 0 = 0
fact' 0 = 1
fact' x = x * fact (x - 1)


h' x
    | x < 0 = 0
    | otherwise = fact'' x
    where
        fact'' :: Integral a => a -> a
        fact'' 0 = 1
        fact'' x = x * fact'' (x-1)

listApp :: [a -> b] -> a -> [b]
listApp [] _     = []
listApp (x:xs) a = x a : listApp xs a

(!=) :: Eq a => a -> a -> Bool
(!=) = (/=)

elagazas :: Integer -> Integer -> Integer
elagazas 0 a = a + 8
elagazas a b
    | a == b             = 1
    | a == 0 && b < 0    = 2
    | a + b == b         = 3
    | False && otherwise = 4
    | b == a + b - a     = 5
    | False || otherwise = 6
    | otherwise          = 7
    
part 0 = [[]]
part n = [n-i : is | i <- [0..n-1], is <- part i]


splitOn' :: Eq a => a -> [a] -> [[a]]
splitOn' e [] = []
splitOn' e l = splitOn1 e l
    where
        splitOn1 e [] = [[]]
        splitOn1 e (x : xs)
            | e == x = [] : splitOn1 e xs
            | otherwise = (x : y) : ys
            where
                (y:ys) = splitOn1 e xs

-- splitOn' végig vezetés:
-- splitOn' 2 [1,2,3,4,2,5,4] ==
-- (1 : (head (splitOn' 2 [2,3,4,2,5,4]))) : (tail (splitOn' 2 [2,3,4,2,5,4])) ==
-- (1 : (head ([] : splitOn' 2 [3,4,2,5,4]))) : (tail ([] : splitOn' 2 [3,4,2,5,4])) ==

-- (1 : (head ([] : ((3 : head (splitOn' 2 [4,2,5,4])) : tail (splitOn' 2 [4,2,5,4]))))) : (tail ([] : ((3 : head (splitOn' 2 [4,2,5,4])) : tail (splitOn' 2 [4,2,5,4])))) == 

-- (1 : (head ([] : ((3 : head ((4 : head (splitOn' 2 [2,5,4])) : tail (splitOn' 2 [2,5,4]))) : tail ((4 : head (splitOn' 2 [2,5,4])) : tail (splitOn' 2 [2,5,4])))))) : (tail ([] : ((3 : head ((4 : head (splitOn' 2 [2,5,4])) : tail (splitOn' 2 [2,5,4]))) : tail ((4 : head (splitOn' 2 [2,5,4])) : tail (splitOn' 2 [2,5,4]))))) ==

-- (1 : (head ([] : ((3 : head ((4 : head ([] : splitOn' 2 [5,4])) : tail ([] : splitOn' 2 [5,4]))) : tail ((4 : head ([] : splitOn' 2 [5,4])) : tail ([] : splitOn' 2 [5,4])))))) : (tail ([] : ((3 : head ((4 : head ([] : splitOn' 2 [5,4])) : tail ([] : splitOn' 2 [5,4]))) : tail ((4 : head ([] : splitOn' 2 [5,4])) : tail ([] : splitOn' 2 [5,4]))))) ==

-- (1 : (head ([] : ((3 : head ((4 : head ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4]))) : tail ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4])))) : tail ((4 : head ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4]))) : tail ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4]))))))) : (tail ([] : ((3 : head ((4 : head ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4]))) : tail ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4])))) : tail ((4 : head ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4]))) : tail ([] : (5 : head (splitOn' 2 [4])) : tail (splitOn' 2 [4])))))) ==

-- (1 : (head ([] : ((3 : head ((4 : head ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 [])))) : tail ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))))) : tail ((4 : head ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 [])))) : tail ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 [])))))))) : (tail ([] : ((3 : head ((4 : head ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 [])))) : tail ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))))) : tail ((4 : head ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 [])))) : tail ([] : (5 : head ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))) : tail ((4 : head (splitOn' 2 [])) : tail (splitOn' 2 []))))))) ==

-- (1 : (head ([] : ((3 : head ((4 : head ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))) : tail ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]])))) : tail ((4 : head ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))) : tail ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))))))) : (tail ([] : ((3 : head ((4 : head ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))) : tail ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]])))) : tail ((4 : head ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))) : tail ([] : (5 : head ((4 : head [[]]) : tail [[]])) : tail ((4 : head [[]]) : tail [[]]))))))

-- Ezekből egy csomó ki fog esni a head és a tail miatt. Ez így viszont már egy konstans kifejezés, még pedig az [[1],[3,4],[5,4]]-é.

{-wordNumWithCapital :: String -> Int
wordNumWithCapital str = sum $ map (\x -> fromEnum $ isUpper $ head x) (words str)-}

minList :: Ord a => [a] -> [a] -> [a]
minList = zipWith min

slow_lucas 0 = 2
slow_lucas 1 = 1
slow_lucas n = slow_lucas (n-1) + slow_lucas (n-2)

lucas :: [Integer]
lucas = 2:1:zipWith (+) lucas (tail lucas)

positiveProduct [] = 1
positiveProduct (x:xs)
    | x > 0 = x * positiveProduct xs
    | otherwise = positiveProduct xs

positiveProduct' = product . filter (> 0)

mightyGale [] = []
mightyGale ((city, _, v, _) : xs)
    | v > 110 = city
    | otherwise = mightyGale xs

validGame l = validGame' (words l)
    where
        validGame' [] = True
        validGame' [_] = True
        validGame' (x:y:xs) = last x == head y && validGame' (y:xs)

listDiff [] _ = []
listDiff a [] = a
listDiff (x:xs) l
    | x `elem` l = listDiff xs l
    | otherwise = x : listDiff xs l

pizza :: [(String,Int)] -> Int
pizza = foldr ((+) . snd) 500

pizza' l = 500 + sum [ar | (_,ar) <- l]

pizza'' [] = 500
pizza'' ((_,ar):xs) = ar + pizza'' xs

repeat' = iterate id

geometricSequence s q = iterate (\x -> x*q) s

data Alma = A Int
    | B
    | C String 
    | D Int deriving (Eq, Show, Ord)
    
{-instance Show Alma where
    show (A a) = show a
    show B     = ""
    show (C str) = str
    show (D a) = show a-}

getA :: Alma -> Int
getA (A b) = b

getC :: Alma -> String
getC (C str) = str

equality :: Alma -> Alma -> Bool
equality a b = a == b

data Answer = No | Maybe | Yes deriving (Eq,Ord,Enum,Show)

data Szilva = K {getInt :: Int, getString :: String, getAnswer :: Answer} deriving Show

fgv :: Szilva -> Szilva
fgv s = s { getInt = 9 }

fgv' :: Szilva -> Szilva
fgv' s = s { getString = "szilva" }
--fgv (K a b) = (K (a+1) b)

fgv2 :: Maybe Szilva -> Maybe Szilva
fgv2 s
    | Just r <- s = Just $ fgv r
    | otherwise = Nothing

fgv3 :: Szilva -> Szilva
fgv3 s = case getAnswer s of
    No    -> fgv' s
    Maybe -> fgv s
    Yes -> fgv' $ fgv s

fgv4 :: Szilva -> Szilva
fgv4 s@(getAnswer -> No) = fgv' s
fgv4 s@(getAnswer -> Maybe) = fgv s
fgv4 s@(getAnswer -> Yes) = fgv $ fgv' s

--{-# LANGUAGE ViewPatterns #-}

both :: a -> (a,a)
both a = (a,a)

data BigData = Big {
    get1 :: Int,
    get2 :: Int,
    get3 :: Int,
    get4 :: Int,
    get5 :: Int,
    get6 :: Int,
    get7 :: Int,
    get8 :: Int,
    get9 :: Int,
    get10 :: Int,
    get11 :: Int,
    get12 :: Int,
    get13 :: Int,
    get14 :: Int,
    get15 :: Int,
    get16 :: Int,
    get17 :: Int,
    get18 :: Int,
    get19 :: Int,
    get20 :: Int,
    get21 :: Int,
    get22 :: Int,
    get23 :: Int }
    | Small {getSmall1 :: Integer, getSmall2 :: Integer}

isBig :: BigData -> Bool
isBig (Big _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = True -- itt sajnos egyszer akkor is végig számolnunk jól a 23 elemet vagy amennyit éppen kell.
isBig _                                                   = False -- de ez tutira akárhány konstruktor esetén működik.
-- ha pont csak kettő van és a másik jóval kisebb, akkor azt lehet mintailleszteni és False-t visszaadni rá.

-- Kell nekünk a nagy adatból a 12. és a 19. elem, de nem biztos, hogy nagyot kapunk

get12_19 (Big _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ y _ _ _ _) = Just (x,y)
get12_19 _                                                   = Nothing

-- Na ezt egy halál végigszámolni egyesével, hogy pont annyi legyen, amennyi kell. Ha 22 vagy 24 van, akkor fordítási hiba, nem jó a mintaillesztés.
-- Ha nem a 12. és a 19. helyeken vannak, akkor nem az elvárt eredményt kapjuk.
-- Egy pár segédfüggvénnyel (both és isBig) leegyszerűsíthetjük az életünket.

get12_19' (both -> ((isBig -> True), both -> (get12 -> x, get19 -> y))) = Just (x,y)
get12_19' _                                                             = Nothing

-- Szemmel láthatóan a kód maga hosszabb, de nem kellett szenvedni azzal, hogy szépen egyesével leszámoljuk az elemeket.
-- Futási költségben közel azonos időt lehet mérni.