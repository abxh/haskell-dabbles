{-# LANGUAGE GADTs #-}

data Peano
  = Zero
  | Succ Peano
  deriving (Show)

toNum :: Peano -> Int
toNum x = toNumInternal 0 x
  where
    toNumInternal acc Zero = acc
    toNumInternal acc (Succ x) = toNumInternal (acc + 1) x

toPeano :: Int -> Peano
toPeano 0 = Zero
toPeano x =
  if x < 0
    then error "Cannot be represented"
    else Succ (toPeano (x - 1))

increment :: Peano -> Peano
increment Zero = Succ Zero
increment x = Succ x

decrement :: Peano -> Peano
decrement Zero = error "Cannot be represented"
decrement (Succ x) = x

add :: Peano -> Peano -> Peano
add Zero y = y
add (Succ x) y = Succ (add x y)

sub :: Peano -> Peano -> Peano
sub Zero Zero = Zero
sub Zero y = error "Cannot be represented"
sub x Zero = x
sub (Succ x) (Succ y) = sub x y

equals :: Peano -> Peano -> Bool
equals Zero Zero = True
equals x Zero = False
equals Zero y = False
equals (Succ x) (Succ y) = equals x y

lt :: Peano -> Peano -> Bool
lt Zero y = True
lt x Zero = False
lt (Succ x) (Succ y) = lt x y

data FunnyType

a :: FunnyType -> FunnyType
a = id

data KindWithAArg a where
  Guh :: a -> KindWithAArg a -- :kind KindWithAArg
  Geh :: a -> KindWithAArg a
  deriving (Eq, Show)

data Price where
  Price :: Integer -> Price
  deriving (Eq, Show)

data Price' =
  Price' Integer deriving (Eq, Show)

data Price'' a =
  Price'' Integer

data Price''' a where
  Price''' :: Integer -> Price''' a
  deriving (Eq, Show)

guhy :: Price''' Bool
guhy = Price''' 5
