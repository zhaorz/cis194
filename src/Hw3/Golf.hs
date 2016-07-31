module Golf where

skips :: [a] -> [[a]]
skips list = [each i list | i <- [1..(length list)]]

each :: Int -> [a] -> [a]
each n list = [list !! (i - 1) | i <- [n, 2 * n .. (length list)]]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs) =
  let rest = localMaxima (z:xs) in
  if x < y && y > z then y:rest
  else rest
localMaxima _ = []

histogram :: [Integer] -> String
histogram l = unlines [line c i | i <- [m, m-1 .. 1]] ++ "==========\n0123456789\n"
  where c = count l
        m = maximum c

line :: [Int] -> Int -> String
line counts n = [if i >= n then '*' else ' ' | i <- counts]

-- count L => L' such that L'[x] is the number of times x occured in L
count :: [Integer] -> [Int]
count l = map (\n -> length $ filter (== n) l) [0..9]
