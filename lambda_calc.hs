{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

data Either a b
  = Left a
  | Right b

type BOOL a b = (a -> b -> Either a b)

true :: BOOL a b
true x y = Left x

false :: BOOL a b
false x y = Right y

ifelse :: BOOL a b -> a -> b -> Either a b
ifelse f x y = f x y

-- TODO: continue this
