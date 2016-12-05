-- 1.2.7
sum1 a b c = a + b + c
lenVec3 x y z =  sqrt (sum1 (x*x) (y*y) (z*z))

-- 1.2.10
sign x = if x > 0
         then 1
         else if x == 0
              then 0
              else -1

-- 1.3.8
infixr 5 |-|
x |-| y = abs (x - y)


-- 1.4.6
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5


-- 1.4.9
import Data.Char
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y
                    then digitToInt(x) * 10 + digitToInt(y)
                    else 100

-- 1.4.11
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2)^2 + (snd p1 - snd p2)^2

--1.5.4
doubleFact :: Integer -> Integer
doubleFact 0 = 1
doubleFact 1 = 1
doubleFact n = n * doubleFact (n - 2)


-- 1.5.8
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n  | n > 0 = fibonacci (n-1) + fibonacci (n-2)
             | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

    
--1.5.10
fib fsum last n | n == 1 = fsum
                | n < 0 = (-1) ^ (1-n) * fib fsum last (-n) 
                | n > 0 = fib (fsum + last) fsum (n - 1)
                | otherwise = undefined 
fibonacci 0 = 0                  
fibonacci n  = fib 1 0 n


-- 1.6.6
seqA :: Integer -> Integer

seqAA a0 a1 a2 n | n == 2 = a2 
                 | n > 2 = seqAA a1 a2 (a2 + a1 - 2 * a0) (n-1)
                 | otherwise = undefined

seqA n | n > 2 = seqAA 1 2 3 n
       | n >= 0 && n < 3 = n + 1
       | otherwise = undefined


-- 1.6.8
sum'n'count :: Integer -> (Integer, Integer)
counting :: Integer -> Integer -> Integer -> (Integer, Integer)

counting x n sumx | x == 0 = (sumx,n)
                  | x > 0 = counting (div x 10) (n+1) (sumx + mod x 10)
                  | otherwise = undefined

sum'n'count x | x == 0 = (0, 1)
              | otherwise = counting (abs x) 0 0


--1.6.9
integration :: (Double -> Double) -> Double -> Double -> Double
trap :: (Double -> Double) -> Double -> Double -> Double -> Double

trap f a h i | i == 1000 = 0
             | otherwise = (f (a + i*h) + f (a + (i+1)*h ))/ 2 * h 
                         + trap f a h (i+1) 

integration f a b = trap f a ((b-a) / 1000) 0 


--2.1.3
getSecondFrom :: a -> b -> c -> b
getSecondFrom _ b _ = b 


--2.1.7
import Data.Function

multSecond = g `on` h
g a b = a * b 
h p = snd p


-- 2.1.9
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


--2.2.3
doItYourself = f . g . h

f = logBase 2 
g = (^3)
h = max 42


--2.3.7
class Printable a where
    toString :: a -> String

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"
 

--2.3.9
instance (Printable a, Printable b) => Printable (a,b) where
    toString p =  "(" ++ toString (fst p) ++"," ++  toString (snd p) ++ ")"



--2.4.3
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | (doesEnrageGork x == True && doesEnrageMork x == True) = stomp (stab x)
                  | (doesEnrageMork x == True) = stomp x
                  | (doesEnrageGork x == True) = stab x
                  | otherwise = x


--2.4.5
a = 127.22
b = 4.12
c = 0.1
d = 2


--2.4.7
class (Eq a, Bounded a, Enum a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | maxBound == x = minBound 
          | otherwise = succ x

  spred :: a -> a
  spred x | minBound == x = maxBound 
          | otherwise = pred x


--2.4.9
avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral(a) + fromIntegral(b) + fromIntegral(c))/3


--3.1.3
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a1 a2 arrs = a1:(a2:arrs)

--3.1.4
nTimes:: a -> Int -> [a]
oneTime _ arrs 0 = arrs
oneTime a arrs n = oneTime a (a:arrs) (n-1)
nTimes a n = oneTime a [] n


--3.1.8
oddsOnly :: Integral a => [a] -> [a]
oddsOnly arrs = filter odd arrs


--3.1.10
isPalindrome :: Eq a => [a] -> Bool

pol [] _ = True
pol (a:arr) (ra:rarr) | a == ra = pol arr rarr
                      | otherwise = False
isPalindrome arr = pol arr (reverse arr)

--3.1.12
sum3 :: Num a => [a] -> [a] -> [a] -> [a]

mysum [] [] res = reverse res
mysum (a:as) [] res = mysum as [] (a:res) 
mysum [] (b:bs) res = mysum [] bs (b:res)
mysum (a:as) (b:bs) res = mysum as bs ((a+b):res)
sum3 as bs cs =  mysum (mysum as bs []) cs []

--3.1.13
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = gE xs [] 1 x

gE [] newList n dig = reverse (replicate n dig : newList)
gE (l:list) newList n dig = if l == dig
                            then gE list newList (n+1) dig
                            else gE list (replicate n dig : newList) 1 l 


--3.2.3
import Data.Char

readDigits :: String -> (String, String)
readDigits str = rd str []

rd [] newList = (reverse newList, [])
rd (s:str) newStr | isDigit s = rd str (s : newStr) 
                  | otherwise = (reverse newStr, s:str)


--3.2.4
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj op1 op2 list =  foldr (\x newList -> if op1 x || op2 x then x:newList else newList) [] list 


--3.2.5
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a <= x] ++ [x] ++ qsort [a | a <- xs, a > x]


--3.2.7
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes list =  foldr (\x newList -> x^2:x^3:newList) [] list 


--3.2.8
perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:xs) = concat (map (\y -> (perms' x y)) (perms xs))
                    where perms' x y =  map (\n -> (take n y) ++ [x] ++ (drop n y)) [0..(length y)]



--3.2.10
import Data.Char
delAllUpper :: String -> String
delAllUpper list = unwords . filter (any (not . isUpper)) . words $ list


--3.2.12
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 a b c = zipWith max a (zipWith max b c)


--3.3.3
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)


--3.3.5
repeatHelper = id


--3.3.7 
-- data Odd = Odd Integer deriving (Eq,Show)
-- не убирайте комментарий с предыдущей строки
-- определение Odd уже присутствует в вызывающей программе
instance Enum Odd where
         succ (Odd x) = Odd (x + 2)
         pred (Odd x) = Odd (x - 2)
         toEnum (x) = (Odd (toInteger x))
         fromEnum (Odd x) = fromInteger x
         enumFrom (Odd x) = (Odd x) : enumFrom (Odd (x + 2))
         enumFromThen (Odd x) (Odd y) = (Odd x) :  enumFromThen (Odd y) (Odd (2*y - x))
         enumFromTo (Odd x) (Odd y) | x == y = [(Odd x)]
                                    | x < y  = (Odd x) : (enumFromTo (Odd (x + 2)) (Odd y))
                                    | x > y  = []
         enumFromThenTo (Odd x) (Odd y) (Odd z) | x == z    = [(Odd x)]
                                                | y == z    = [(Odd x), (Odd y)]
                                                | (y > z && x < y)  || (y < z && x > y)  = [(Odd x)]
                                                | otherwise = (Odd x) : enumFromThenTo (Odd y) (Odd (y + (y - x))) (Odd z)


--3.3.9
change :: (Ord a, Num a) => a -> [[a]] 
change n | n == 0 = [[]]
         | otherwise = [c:change' | c <- coins, c <= n, change' <- change (n - c)]


--3.4.3
concatList :: [[a]] -> [a]
concatList = foldr (++) [] 

--3.4.5
lengthList :: [a] -> Int
lengthList = foldr (\x len -> 1 + len) 0 

--3.4.6
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0


--3.5.8
meanList :: [Double] -> Double
meanList a = (foldr (\x s -> x + s) 0 a) / (realToFrac (length a))


--3.5.9
evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly (a:[]) = []
evenOnly (a:b:lst) = b:evenOnly lst


--3.5.10
evenOnly :: [a] -> [a]
evenOnly [] = []
evenOnly (a:[]) = []
evenOnly (a:b:lst) = b:evenOnly lst

--3.6.3
lastElem :: [a] -> a
lastElem xs = foldl1 (\a b -> b) xs

--3.6.10
import Data.Char
revRange :: (Char,Char) -> [Char]
revRange (x1, x2) = unfoldr g x2
  where g a | a >= x1 = Just (a, chr(ord(a) - 1))
            | otherwise = Nothing
            

--4.1.5
instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"


--4.1.7
charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9



--4.1.8
data Color = Red | Green | Blue

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue


--4.1.11
cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Info = GT
cmp Warning Warning = EQ
cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Error = LT
cmp Info Error = LT
cmp Info Warning = LT

--4.1.13
processData :: SomeData -> String
processData x = case (snd (doSomeWork x)) of
                                  0 -> "Success"    
                                  _ -> "Fail: " ++ show (snd (doSomeWork x))
           

--4.2.3
data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)


--4.2.5
data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Rectangle a b) = a*b
area (Circle r) = pi*r*r


--4.2.6
data Result' = Fail' Int | Success' 

instance Show Result' where
    show Success' = "Success"
    show (Fail' x) = "Fail: " ++ (show x)

doSomeWork' :: SomeData -> Result'
doSomeWork' x | snd (doSomeWork x) == 0 = Success' 
              | otherwise = Fail' (snd (doSomeWork x))


--4.2.8
data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle a b) | a == b = True
isSquare _ = False


--4.2.9
data Bit = Zero | One deriving Show
data Sign = Minus | Plus deriving Show
data Z = Z Sign [Bit] deriving Show

oneBitToInt :: Bit -> Integer
oneBitToInt Zero = 0
oneBitToInt One = 1

oneIntToBit :: Integer -> Bit
oneIntToBit 0 = Zero
oneIntToBit 1 = One

translate :: Z -> Integer -> Integer
translate (Z Plus []) _ = 0
translate (Z Plus (x:xs)) n = (oneBitToInt x) * (2^n) + (translate (Z Plus (xs)) (n + 1))


toBits :: Integer -> [Bit]
toBits 0 = [Zero]
toBits n | n > 0 = (oneIntToBit (mod n 2)) : toBits (div n 2)

toInt :: Z -> Integer
toInt (Z _ []) = 0
toInt (Z Plus (x:xs)) = translate (Z Plus (x:xs)) 0
toInt (Z Minus x) = - (toInt (Z Plus x))


toZ :: Integer -> Z
toZ 0 = (Z Plus [])
toZ n | n > 0 = (Z Plus (toBits n))
      | otherwise = (Z Minus (toBits (-n))) 

add :: Z -> Z -> Z
add x y = toZ ((toInt x) + (toInt y))

mul :: Z -> Z -> Z
mul x y = toZ ((toInt x) * (toInt y))


--4.3.3
import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString (LogEntry a b c)= (timeToString a) ++ ": " ++ (logLevelToString b) ++ ": " ++ c


--4.3.5
data Person = Person { firstName :: String, lastName :: String, age :: Int }

updateLastName :: Person -> Person -> Person
updateLastName (Person f l a) person = person {lastName = l} 


--4.3.8
data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName x | length (firstName x) < 2 =  x
                | otherwise = x {firstName = [(head (firstName x))] ++ "."}


--4.4.3
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs(y1 - y2)


--4.4.4
data Coord a = Coord a a

getCenter :: Double -> Coord Int -> Coord Double
getCenter a (Coord x y) = (Coord ((fromIntegral x)*a + a/2) ((fromIntegral y)*a + a/2))

getCell :: Double -> Coord Double -> Coord Int
getCell a (Coord x y) = (Coord (floor (x/a)) (floor (y/a)))


--4.4.6
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs) | (isDigit x == True) = Just x
                 | (xs == []) = Nothing
                 | otherwise = findDigit xs


--4.4.7
import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX l = case findDigit l of
                    Nothing -> 'X'
                    Just x -> x

--4.4.8
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x


--4.4.12
eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing

--4.5.3
data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons a b) = a : (fromList b)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)


--4.5.4
data Nat = Zero | Suc Nat deriving Show

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add x y = toNat ((fromNat x) + (fromNat y))

mul :: Nat -> Nat -> Nat
mul x y = toNat ((fromNat x) * (fromNat y))

fact :: Integer -> Integer
fact n = product [1..n]

fac :: Nat -> Nat
fac x = toNat(fact(fromNat x))


--4.5.5
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (Leaf a) = 0
height (Node l r) = 1 + max (height l)  (height r)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node r l) = 1 + size l + size r


--4.5.6
data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf a) = (1, a)
    go (Node x y) = (fst (go x) + fst (go y), snd (go x) + snd (go y))


--4.5.8
infixl 6 :+:
infixl 7 :*:
data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
    deriving (Show, Eq)

expand :: Expr -> Expr
expand e = if e == expanded 
           then expanded 
           else expand expanded
  where
    expanded = expand' e

    expand' :: Expr -> Expr
    expand' ((e1 :+: e2) :*: e) =  expand e1 :*: expand e :+: expand e2 :*: expand e
    expand' (e :*: (e1 :+: e2)) =  expand e :*: expand e1 :+: expand e :*: expand e2

    expand' ((e :+: e1) :+: e2) =  expand e :+: (expand e1 :+: expand e2)
    expand' ((e :*: e1) :*: e2) =  expand e :*: (expand e1 :*: expand e2)

    expand' (e1 :+: e2)         =  expand e1 :+: expand e2
    expand' (e1 :*: e2)         =  expand e1 :*: expand e2
    expand' e                   =  e


--5.1.3
instance Functor Point3D where
    fmap f (Point3D x y z) = (Point3D (f x) (f y) (f z))


--5.1.4
instance Functor GeomPrimitive where
    fmap f (Point (Point3D x y z)) = (Point (Point3D (f x) (f y) (f z)))
    fmap f (LineSegment (Point3D x1 y1 z1) (Point3D x2 y2 z2)) = LineSegment (fmap f (Point3D x1 y1 z1)) (fmap f (Point3D x2 y2 z2))


--5.1.6
instance Functor Tree where
    fmap f (Leaf (Nothing)) = (Leaf (Nothing))
    fmap f (Leaf (Just x)) = (Leaf(Just(f x)))
    fmap f (Branch l x r) = (Branch (fmap f l) (fmap f x) (fmap f r))


--5.1.8
instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map xs) = Map (map (\x -> fmap f x) xs)

--5.2.3
toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f  msg = \x -> (Log [msg] (f x))

strLog :: Log a -> [String]
strLog (Log [msg] x) = [msg]

rsltLog ::  Log b -> b
rsltLog (Log [msg] x) = x

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = (Log  (strLog (f x) ++  strLog (g (rsltLog (f x)))) (rsltLog (g (rsltLog(f x))) ))


--5.2.5
returnLog :: a -> Log a
returnLog x = (Log [] x)


--5.2.7
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log str a) f = Log (str++str2) a2
                            where Log str2 a2 = f a


--5.2.8
execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a fs = foldl (>>=) (return a) fs


--5.3.3
instance Functor SomeType where
    fmap f x = x >>= return . f


--5.4.4
-- data Token = Number Int | Plus | Minus | LeftBrace | RightBrace     
--     deriving (Eq, Show)
-- Тип Token уже объявлен, его писать не нужно

asToken :: String -> (Maybe Token)
asToken s | all isDigit s = (Just (Number (read s :: Int)))
          | s == "+" = (Just Plus)
          | s == "-" = Just Minus
          | s == "(" = Just LeftBrace
          | s == ")" = Just RightBrace
          | otherwise = Nothing

tokenize :: String ->  Maybe [Token]
tokenize input = do
      let i = words input
      let mbstr =  [asToken x | x <- i]
      sequence mbstr


--5.4.6
--Тип Board и функция nextPositions заданы, реализовывать их не нужно

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0 = []
                        | n == 0 && (pred b) = [b]
                        | n == 0 = []
                        | otherwise = do
                            x <- nextPositions b
                            nextPositionsN x (n - 1) pred


--5.4.8
pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
                    | otherwise = do
                         a <- [1,2..x]
                         b <- [1,2..(a - 1)]
                         c <- [1,2..x]
                         True <- return (a^2 + b^2 == c^2)
                         return (b, a, c)
                         


--5.5.3
main' :: IO ()
main' = do 
     putStrLn "What is your name?"
     putStr "Name: "
     name <- getLine
     if (name == "") 
     then main' 
     else  putStrLn ("Hi, " ++ name ++ "!")
     


--5.5.8
import Data.List
main' :: IO ()
main' = do
      putStr "Substring: "
      substr <- getLine
      if substr == [] then putStrLn "Canceled" else do
          dir <- getDirectoryContents "."
          let dirdir = filter (isInfixOf substr) dir
          mapM_ (\x -> putStrLn ("Removing file: " ++ x) >> removeFile x) dirdir


--5.6.9
local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader $ \e -> runReader m (f e)


--5.6.9
usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = do
            e <- asks (filter (\x -> 
                                    if snd x == "123456"
                                    then True
                                    else False))
            return $ map (fst) e




