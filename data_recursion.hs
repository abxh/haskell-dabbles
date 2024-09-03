
f :: a -> [a]
f x = x : f x


-- following is possible because of lazy eval
-- 1 -> 1 -> ... -> f 1
ones = f 1

-- following is (also) possible because of lazy eval:
--  ______
-- |      |
-- |->[2][_]
--
-- Latter is more efficient memory wise
twos = 2 : twos
