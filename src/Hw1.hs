module Hw1
  ( toDigits
  , toDigitsRev
  , doubleEveryOther
  , sumDigits
  , validate
  , hanoi
  ) where

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0     = []
  | n == 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (a:[])   = [a]
doubleEveryOtherRev (a:b:xs) = a:(b * 2):doubleEveryOtherRev xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = reverse (doubleEveryOtherRev (reverse l))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)
