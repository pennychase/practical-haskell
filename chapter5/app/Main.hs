module Main where

main :: IO ()
main = do
  putStrLn $ show result

result :: Integer
result = foldr (*) 1 [1 .. 10000]

