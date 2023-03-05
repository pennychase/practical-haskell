module Chapter5.Exercises where


-- Sieve of Eratosthenes

filterMultiples :: Integer -> [Integer] -> [Integer]
filterMultiples n ns = filter (\x -> x `mod` n /= 0) ns

sieve :: [Integer] -> [Integer]
sieve (n:ns) = 
    let
        filtered = filterMultiples n ns
    in n : sieve filtered

primes :: [Integer]
primes = sieve [2..]