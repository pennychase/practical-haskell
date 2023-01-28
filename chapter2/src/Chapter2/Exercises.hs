module Chapter2.Exercises where

-- Exercise 2.6

ackerman :: Integer -> Integer -> Integer
ackerman m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = ackerman (m - 1) 1
    | m > 0 && n > 0 = ackerman (m - 1) (ackerman m (n - 1))

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y) : rest) = 
    let
        (xs, ys) = myUnzip rest
    in (x:xs, y:ys)