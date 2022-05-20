data NaturalNumber = Zero | S NaturalNumber
  deriving (Show)

-- common names for small natural numbers

zero :: NaturalNumber
zero = Zero

one :: NaturalNumber
one = S zero

two :: NaturalNumber
two = S one

three :: NaturalNumber
three = S two

four :: NaturalNumber
four = S three

five :: NaturalNumber
five = S four

six :: NaturalNumber
six = S five

seven :: NaturalNumber
seven = S six

eight :: NaturalNumber
eight = S seven

nine :: NaturalNumber
nine = S eight

ten :: NaturalNumber
ten = S nine

instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S _ = False
    S _ == Zero = False
    S x == S y = x == y

infinity = S infinity 

-- Exercise 1
instance Ord NaturalNumber where
  compare Zero Zero = EQ
  compare  Zero (S _) = LT
  compare (S _) Zero = GT
  compare (S Zero) (S _) = LT
  compare (S (S _)) (S _) = GT