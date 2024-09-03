import Data.Char

predCustom :: Char -> Char
predCustom 'A' = 'Z'
predCustom 'a' = 'z'
predCustom c = pred c

succCustom :: Char -> Char
succCustom 'Z' = 'A'
succCustom 'z' = 'a'
succCustom c = succ c

caeser :: Int -> [Char] -> [Char]
caeser 0 s = s
caeser o s
  | o < 0 = caeser (o + 1) $ map predCustom s
  | otherwise = caeser (o - 1) $ map succCustom s

uncaeser :: Int -> [Char] -> [Char]
uncaeser o = caeser (-o)
