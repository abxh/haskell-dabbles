-- Expected to be imported
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use section" #-}

import Data.Char

squareInt :: Integer -> Integer
squareInt x = x * x

square :: Num a' => a' -> a'
square x = x * x

mul :: Num a' => a' -> a' -> a'
mul x y = x * y

isFive :: Integer -> Bool
isFive 5 = True
isFive x = False

nand :: Bool -> Bool -> Bool
nand True True = False
nand x y = True

-- Main.fst
fst :: (a, b) -> a -- note the different letters here
fst (x, y) = x

-- Main.snd
snd :: (a, b) -> b
snd (x, y) = y

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty (x : xs) = False

-- Main.head
-- head :: [a] -> a
-- head [] = error "empty list"
-- head (x : xs) = x

-- Main.tail
-- tail :: [a] -> [a]
-- tail [] = []
-- tail (x : xs) = xs

is_a :: Char -> Bool
is_a 'a' = True
is_a x = False

is_hello :: String -> Bool
is_hello "Hello" = True
is_hello "hello" = True
is_hello x = False

remove_leading_whitespace :: String -> String
remove_leading_whitespace [] = ""
remove_leading_whitespace (' ' : xs) = xs
remove_leading_whitespace (x : xs) = x : xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

add_n :: Integer -> Integer -> Integer
add_n n x = x + n

add_three :: Integer -> Integer
add_three = add_n 3 -- same as: add_three x = add_n 3 x

abs :: Integer -> Integer
abs x =
  if x >= 0
    then x
    else -x

quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c =
  let d = b ^ 2 - 4 * a * c
      x1 = (-b + sqrt d) / (2 * a)
      x2 = (-b - sqrt d) / (2 * a)
   in (x1, x2)

get_3rd_elm :: [a] -> a
get_3rd_elm l =
  (!!) l 2 -- index operator. 0-indexed

at_index :: [a] -> Int -> a
at_index = (!!)

get_last_elm :: [a] -> a
get_last_elm [] = error "empty list"
get_last_elm l = l `at_index` (length l - 1)

take_2 :: [a] -> [a]
take_2 = take 2

drop_2 :: [a] -> [a]
drop_2 = drop 2

take_last_2 :: [a] -> [a]
take_last_2 l =
  take 2 (reverse l)

append_to_front :: [a] -> a -> [a]
append_to_front l e = e : l

append_to_back :: [a] -> [a] -> [a]
append_to_back = (++)

square_all :: Num a => [a] -> [a]
square_all =
  map square

bool_to_int_rep :: Bool -> Int
bool_to_int_rep False = 0
bool_to_int_rep True = 1

int_to_bool_rep :: Int -> Bool
int_to_bool_rep 0 = False
int_to_bool_rep x = True

bool_list_to_int_rep :: [Bool] -> [Int]
bool_list_to_int_rep = map bool_to_int_rep

int_list_to_bool_rep :: [Int] -> [Bool]
int_list_to_bool_rep = map int_to_bool_rep

contains_0_char :: [Char] -> Bool -- [Char] == String
contains_0_char l =
  or (map ((==) '0') l) -- alt: any ((==) '0') l, elem '0' l

-- note:
-- zip iterates with the smallest list length of the two

gen_alph_pairs :: Int -> [(Int, Char)]
gen_alph_pairs n =
  zip [1, 2 .. n] ['a', 'b' .. 'z']

gen_squares :: Int -> [Int]
gen_squares n =
  zipWith (*) [1, 2 .. n] [1, 2 .. n]

subl :: [Int] -> Int
subl = foldl (-) 0

-- subl [1,2,3]
-- =>       (- 0 1)
-- =>    (- (- 0 1) 2)
-- => (- (- (- 0 1) 2) 3)
-- => -6

subr :: [Int] -> Int
subr = foldr (-) 0

-- subr [1,2,3]
-- =>           (- 1 0)  -- 1
-- =>      (- 2 (- 1 0)) -- 2 - 1 = 1
-- => (- 3 (- 2 (- 1 0)) -- 3 - 1
-- => 2

-- `cons`truct list
cons :: a -> [a] -> [a]
cons = (:)

contains_odd :: (Foldable t, Integral a) => t a -> Bool
contains_odd = foldr ((||) . (== 1) . (`mod` 2)) False

quadEqDiscrim :: Float -> Float -> Float -> Float
quadEqDiscrim a b c = b * b - 4 * a * c

quadEqSolution :: Float -> Float -> Float -> (Maybe Float, Maybe Float)
quadEqSolution a b c
  | d > 0 = (Just ((-b + sqrt d) / (2 * a)), Just ((-b - sqrt d) / (2 * a)))
  | d == 0 = (Just (-b / (2 * a)), Nothing)
  | otherwise = (Nothing, Nothing)
  where
    d = quadEqDiscrim a b c

detect :: (Show a, Eq a) => a -> [a] -> String
detect value list =
  if value `elem` list
    then "List contains " ++ show value
    else "List does not contain " ++ show value

showMaybe :: Show a => Maybe a -> String
showMaybe Nothing = ""
showMaybe (Just x) = show x

bitAnd :: Int -> Int -> Int
bitAnd 1 1 = 1
bitAnd x y = 0

bitwiseAnd :: [Int] -> [Int] -> [Int]
bitwiseAnd = zipWith bitAnd

maybeJustList :: [Maybe a] -> [a]
maybeJustList l =
  [x | Just x <- l]

largerThanList :: Int -> [Int] -> [Int]
largerThanList n l =
  [x | x <- l, x > n]

whereAppearsInList :: [Int] -> Int -> [Int]
whereAppearsInList l n =
  [i | i <- [0 .. length l - 1], l !! i == n]

notSquaresUpTo20 :: [Int]
notSquaresUpTo20 =
  [x | x <- [1 .. 20], x `notElem` ([z * z | z <- [1 .. truncate (sqrt 20)]])]

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

countLetterInStr :: Char -> String -> Int
countLetterInStr c = foldr ((+) . boolToInt . (==) c) 0

ifLetterThenDontAppend :: Char -> Char -> String -> String
ifLetterThenDontAppend c c' s =
  if c' == c
    then s
    else c' : s

removeLetterFromStr :: Char -> String -> String
removeLetterFromStr c = foldr (ifLetterThenDontAppend c) []

appendToEnd :: [a] -> a -> [a]
appendToEnd a e = e : a

rev :: [a] -> [a]
rev = foldl appendToEnd []

justElm :: [a] -> a -> [a]
justElm a e = [e]

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast l = Just $ head $ rev l

-- f :: (a -> b) -> a -> b
-- f $ a = f a
-- note precendence with :info (it has the lowest precedence! [aka it is evaluated first])

addOneIfOdd n =
  case odd n of
    True -> f n
    False -> n
  where
    f n = n + 1

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

returnLast' :: a -> (b -> (c -> (d -> d)))
returnLast' _ _ _ d = d

