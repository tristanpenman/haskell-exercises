
--
-- Exercise 1
--
-- TODO: My intuition tells me that toDigitsRev will be faster because it is
--       cons'ing an item and a list, rather than concatenating two lists,
--       but I'm not sure...
--

toDigits :: Integer -> [Integer]
toDigits number
    | number > 0 = toDigits (number `div` 10) ++ [number `mod` 10]
    | otherwise  = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev number
    | number > 0 = (number `mod` 10) : toDigitsRev (number `div` 10)
    | otherwise  = []

--
-- Exercise 2
--
-- TODO: Not entirely satisfied with this... is there any overhead to calling
--       reverse twice? Can it be done without reversing the list?
--

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFwd . reverse
    where doubleFwd :: [Integer] -> [Integer]
          doubleFwd [] = []
          doubleFwd [n] = [n]
          doubleFwd (a:b:cs) = a : (2*b) : doubleFwd cs

--
-- Exercise 3
--

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
    | x > 9      = x - 9 + sumDigits xs
    | otherwise  = x + sumDigits xs

--
-- Exercise 4
--
-- TODO: this can probably be improved with . or $
--

validate :: Integer -> Bool
validate number = summed `mod` 10 == 0
    where digits = toDigits number
          everyOtherDoubled = doubleEveryOther digits
          summed = sumDigits everyOtherDoubled

--
-- Exercise 5
--

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = left ++ [(a, b)] ++ right
  where left  = hanoi (n - 1) a c b
        right = hanoi (n - 1) c b a
