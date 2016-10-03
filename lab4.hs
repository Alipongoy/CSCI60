-- CSci 60 Lab 4

-- The Haskell Prelude has these operations related to what we've been
-- talking about in class recently (all valid for Integral a)
--
--  quot :: a -> a -> a                  (truncate towards 0)
--  rem :: a -> a -> a                   (remainder for quot)
--  div :: a -> a -> a                   (truncate towards -oo)
--  mod :: a -> a -> a                   (remainder for div)
--  quotRem :: a -> a -> (a, a)          (pair of quot, rem)
--  divMod :: a -> a -> (a, a)           (pair of div, mod)
--  gcd :: a -> a -> a                   (implements Euclid's algorithm)


-- Extended GCD.
-- Inputs: a >= 0, b >= 0, not both 0 (you don't need to check this)
-- Outputs: (d,m,n) such that d = gcd(a,b) and m*a + n*b = d
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (d, m, n) = extGCD b (rem a b)
                 (q, r) = quotRem a b
             in (d, n, m - q * n)



-- Multiplicative inverse of a mod m.
-- Inputs: a > 0, m > 1 such that gcd(a,m) = 1 (you don't need to check this)
-- Outputs: b such that 0 < b < m and a * b == 1 (mod m)
minv :: Integer -> Integer -> Integer
minv a m = let (_, x, _) = extGCD a m
           in x



-- Here is an inefficient (but easier to understand) definition of the
-- infinite list of all prime numbers
primes_slow = sieve [2..] where
  sieve (p:xs) = p : sieve [x | x <- xs, rem x p /= 0]

-- Here is a much more efficient (but harder to understand) version of primes.
-- Try "take 100 primes" as an example (or even more if you like)
primes = 2 : primesFrom3 where 
    primesFrom3 = sieve [3,5..] 9 primesFrom3
    sieve (x:xs) b ~ps@(p:q:_)
      | x < b     = x : sieve xs b ps
      | otherwise = sieve [x | x <- xs, rem x p /= 0] (q^2) (tail ps)


-- Write a function that factors its first argument using the (infinite)
-- list of available factors given in its second argument
-- (using rem x p /= 0 to check divisibility)
primeFactsWith :: Integer -> [Integer] -> [Integer]
primeFactsWith n (p:ps) | ((length ps == 0) && (n `mod` p /= 0)) = []
                        | ((length ps == 0) && (n `mod` p == 0)) = [p]
                        | (n `mod` p == 0) = p:(primeFactsWith (div n p) (p:ps))
                        | (n `mod` p /= 0) = primeFactsWith n ps
                        -- | otherwise = [42]


-- Then use the above function to get the actual prime factorization of a number
primeFacts :: Integer -> [Integer]
primeFacts n = let primelist = takeWhile (n>=) primes
               in primeFactsWith n primelist 