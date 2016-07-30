module Hw1
  ( toDigits
  , toDigitsRev
  ) where

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0     = []
  | n == 0    = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)
