-- From:
-- https://stackoverflow.com/a/775795

-- (Following is akin to yield statements)

primes :: [Integer]
primes = primes_find [2..]
    where 
        primes_find [] = []
        primes_find (p:xs) = p : primes_find [x | x<-xs, x `mod` p /= 0]

foo = takeWhile (< 100) primes
