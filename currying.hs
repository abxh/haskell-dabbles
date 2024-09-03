--

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b
