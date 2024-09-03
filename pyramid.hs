import Data.Semigroup (stimes)

pyramid :: Int -> String
pyramid n =
  concat
    [ foldr
        (:)
        (foldr (:) "\n" (stimes (2 * x - 1) ['#']))
        (replicate (n - x) ' ')
      | x <- [1 .. n]
    ]
