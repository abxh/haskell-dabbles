quotRemTerm x y =
  (quot x y) * y + (rem x y)

divModTerm x y =
  (div x y) * y + (mod x y)

type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

type Remainder = Integer

dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy x y
  | y == 0 = error "division by zero!"
  | otherwise = dividedBy' x y 0
  where
    dividedBy' x y count
      | x < y = (count, x)
      | otherwise = dividedBy' (x - y) y (count + 1)
