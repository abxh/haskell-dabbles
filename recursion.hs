{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

import Data.Char

factorial :: Int -> Int
factorial n =
  if n >= 0
    then factorial' 1 n
    else error "Factorial of negative number is undefined!"
  where
    factorial' :: Int -> Int -> Int
    factorial' acc 0 = acc
    factorial' acc n = factorial' (acc * n) (n - 1)

lengthInner :: Int -> [a] -> Int
lengthInner acc [] = acc
lengthInner acc (x : xs) = lengthInner (acc + 1) xs

length :: [a] -> Int
length = lengthInner 0

rsum :: Num a => [a] -> a
rsum [] = 0
rsum (x : xs) = 1 + rsum xs

tsum :: Num a => [a] -> a
tsum l = tsumh l 0
  where
    tsumh [] acc = acc
    tsumh (x : xs) acc = tsumh xs (x + acc)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs Main.++ ys)

zip :: [a] -> [b] -> [(a, b)]
zip [] ys = []
zip xs [] = []
zip (x : xs) (y : ys) = (x, y) : Main.zip xs ys

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs Prelude.++ Main.concat xss

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (splitter : xs) =
  quicksort [y | y <- xs, y < splitter]
    Prelude.++ [splitter]
    Prelude.++ quicksort [y | y <- xs, y >= splitter]

copy :: [a] -> [a]
copy [] = []
copy (x : xs) = x : copy xs

inverse :: [(a, b)] -> [(b, a)]
inverse [] = []
inverse ((x0, x1) : xs) = (x1, x0) : inverse xs

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge (x : xs) (y : ys) =
  if x <= y
    then x : merge xs (y : ys)
    else y : merge ys (x : xs)

(!!) :: [a] -> Int -> Maybe a
[] !! i = Nothing
(x : xs) !! 0 = Just x
(x : xs) !! i =
  if i < Prelude.length (x : xs)
    then xs Main.!! (i - 1)
    else Nothing

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup x [] = Nothing
lookup x ((x0, x1) : xs) =
  if x == x0
    then Just x1
    else Main.lookup x xs

countElmCount :: Eq a => a -> [a] -> Int
countElmCount e [] = 0
countElmCount e (x : xs) =
  (if e == x then 1 else 0) + countElmCount e xs

removeAllOccurencesOf :: Eq a => a -> [a] -> [a]
removeAllOccurencesOf e [] = []
removeAllOccurencesOf e (x : xs) =
  if e == x
    then removeAllOccurencesOf e xs
    else x : removeAllOccurencesOf e xs

removeAltElm :: [a] -> [a]
removeAltElm [] = []
removeAltElm [x] = []
removeAltElm (x0 : (x1 : xs)) = x1 : removeAltElm xs

extract :: [Maybe a] -> [a]
extract [] = []
extract (Just x : xs) = x : extract xs
extract (Nothing : xs) = extract xs

substrIn :: String -> String -> Bool
substrIn [] [] = True
substrIn x [] = True
substrIn [] y = False
substrIn (x : xs) (y : ys) =
  (x == y) && substrIn xs ys

substrIndex :: String -> String -> Maybe Int
substrIndex x y =
  substrIndexInternal x y 0
  where
    substrIndexInternal [] y i = Nothing
    substrIndexInternal (x : xs) y i =
      if substrIn (x : xs) y
        then Just i
        else substrIndexInternal xs y (i + 1)

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : Main.map f xs

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f [] y = []
zipWith f x [] = []
zipWith f (x : xs) (y : ys) =
  f x y : Main.zipWith f xs ys

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x : xs) = f x (Main.foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x : xs) = f (Main.foldl f z xs) x

factorial'' :: Int -> Int
factorial'' n = Main.foldr (*) 1 [1 .. n]

-- firsts :: [(a, b)] -> []
-- firsts [] = []
-- firsts ((a, b) : ps) = a : firsts ps

firsts :: [(a, b)] -> [a]
firsts = Prelude.map fst

foldrWith :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldrWith f c x [] = c
foldrWith f c [] y = c
foldrWith f c (x : xs) (y : ys) =
  f x y (foldrWith f c xs ys)

-- mappend f xs = concat (map f xs)

-- Prelude.mappend :: Monoid a => a -> a -> a
mappend :: (a -> [b]) -> [a] -> [b]
mappend f [] = []
mappend f (x : xs) =
  f x Prelude.++ Main.mappend f xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (removeDuplicatesInternal xs x)
  where
    removeDuplicatesInternal [] e = []
    removeDuplicatesInternal (x : xs) e =
      if x == e
        then removeDuplicatesInternal xs e
        else x : removeDuplicatesInternal xs e

elm :: Eq a => a -> [a] -> Bool
elm e [] = False
elm e (x : xs) = (e == x) || elm e xs

intersection :: Eq a => [a] -> [a] -> [a]
intersection [] [] = []
intersection x [] = []
intersection [] y = []
intersection (x : xs) (y : ys) =
  if x == y
    then x : intersection xs ys
    else intersection xs ys

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset [] [] = True
isSubset x [] = True
isSubset [] y = False
isSubset (x : xs) (y : ys) =
  x == y && isSubset xs ys

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x0 : (x1 : xs)) = x0 <= x1 && isSorted (x1 : xs)

last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (x0 : (x1 : xs)) = Main.last (x1 : xs)

parseDecimal :: String -> (Int, Int)
parseDecimal s = (wholePart s, fracPart s)

wholePart :: String -> Int
wholePart s = wholePartInternal s 0
  where
    wholePartInternal [] acc = acc
    wholePartInternal (x : xs) acc
      | x == '.' = acc
      | isDigit x = wholePartInternal xs ((acc * 10) + digitToInt x)
      | otherwise = error "Not digit!"

fracPart :: String -> Int
fracPart s = fracPartInternal s 0 False
  where
    fracPartInternal [] acc _ = acc
    fracPartInternal ('.' : xs) acc False = fracPartInternal xs acc True
    fracPartInternal (_ : xs) acc False = fracPartInternal xs acc False
    fracPartInternal ('.' : xs) acc True = error "two dots! ambigious"
    fracPartInternal (x : xs) acc True
      | isDigit x = fracPartInternal xs (10 * acc + digitToInt x) True
      | otherwise = error "Not digit!"

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum [x] = x
maximum (x0 : (x1 : xs)) =
  if x0 >= x1
    then Main.maximum (x0 : xs)
    else Main.maximum (x1 : xs)

and :: [Bool] -> Bool
and = Main.foldr (&&) True

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = isPalindrome' xs (reverse xs)
  where
    isPalindrome' [] [] = True
    isPalindrome' xs [] = error "unexpected"
    isPalindrome' [] ys = error "unexpected"
    isPalindrome' (x : xs) (y : ys) = x == y && isPalindrome' xs ys

splitAtSpace :: String -> [String]
splitAtSpace "" = []
splitAtSpace s = tok : splitAtSpace next
  where
    s' = dropWhile (== ' ') s
    tok = takeWhile (/= ' ') s'
    next = dropWhile (/= ' ') s'

splitAtNewline :: String -> [String]
splitAtNewline "" = []
splitAtNewline s = tok : splitAtNewline next
  where
    s' = dropWhile (== '\n') s
    tok = takeWhile (/= '\n') s'
    next = dropWhile (/= '\n') s'
