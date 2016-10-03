-- Joshua Jardenil
-- 12:00 PM
-- CSci 60 Lab 5

---------------- Given helper functions -------------------------------
-- Extended GCD -- usage: extGCD a b = (d, m, n)
-- assumes: a >= 0, b >= 0, not both 0
-- assures: d = gcd(a,b), m*a + n*b = d
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (q, r) = divMod a b
                 (d, m, n) = extGCD b r
						 in (d, n, m - (q * n))

-- Coprime with a list
-- usage: coprime_with x xs = ans
-- assumes: nothing
-- assures: ans == True, if x is coprime with each element of xs
--          ans == False, otherwise
coprime_with :: Integer -> [Integer] -> Bool
coprime_with x [] = True
coprime_with x (y:ys) = gcd x y == 1 && coprime_with x ys

-- Pairwise Coprime
-- usage: pairwise_coprime xs = ans
-- assumes: nothing
-- assures: ans == True, if the elements of the list xs are pairwise coprime
--          ans == False, otherwise
pairwise_coprime :: [Integer] -> Bool
pairwise_coprime [] = True
pairwise_coprime (x:xs) = coprime_with x xs && pairwise_coprime xs

-- Multiplicative inverse
-- usage: minv a m = ainv
-- assumes: a >= 0, m >= 2, and gcd(a,m) = 1
-- assures: a * ainv ≡ 1 (mod m).
minv :: Integer -> Integer -> Integer
minv a m = let (1, ainv, _) = extGCD a m
           in ainv `mod` m

---------------- End of given functions -------------------------------


-- Does a given Chinese Remainder Theorem problem have a solution?
-- usage: crtHasSolution as ms = ans
-- assures: ans == True, if
--                  (1) the lists as and ms are the same length and nonempty,
--                  (2) each element ai of as satisfies ai >= 0,
--                  (3) each element mi of ms satisfies mi >= 2, and
--                  (4) the elements of ms are pairwise coprime;
--          ans == False, otherwise.
crtHasSolution :: [Integer] -> [Integer] -> Bool
crtHasSolution as ms = (length as == length ms) && (length as /= 0 && length ms /= 0) && (all (>=0) as) && (all (>=2) ms) && (pairwise_coprime ms)


-- Is a given number a solution to a CRT problem?
-- usage: crtIsSolution n as ms = ans
-- assures: ans == True, if crtHasSolution as ms == True and n is a solution
--          ans == False, otherwise
crtIsSolution :: Integer -> [Integer] -> [Integer] -> Bool
crtIsSolution n as ms = (crtHasSolution as ms) && (crt as ms == n)


-- Chinese Remaninder Theorem
-- usage: crt as ms = ans
-- assumes: nothing
-- assures: ans == Nothing, if crtHasSolution as ms == False
--          ans == Just n, if n is such that crtIsSolution n as ms == True
crt :: [Integer] -> [Integer] -> Integer
crt as ms | crtHasSolution as ms == False = undefined
          | otherwise = let bigB = foldl (*) (head ms) (tail ms)
                            blist = map (bigB `div`) ms
                            xlist = zipWith (minv) blist ms
                            multiplicativeResult = zipWith (*) (zipWith (*) xlist as) blist
                            additionResult = foldl (+) (head multiplicativeResult) (tail multiplicativeResult)
                            in (minv additionResult bigB)


-- Implement (efficient) modular exponentiation
-- usage: expmod a n m = b
-- assumes: n >= 0, m >= 2
-- assures: a^n ≡ b (mod m), 0 <= b < m
-- expmod :: Integer -> Integer -> Integer -> Integer
-- expmod a n m = let n = 
-- I tried :)


-- Convert number to base b
-- usage: toBase b n = xs
-- assumes: b >= 2, n >= 0
-- assures: xs = digits of n in base b, from most significant to least 
toBase :: Integer -> Integer -> [Integer]
toBase b n | b > n = [n]
           | otherwise = (toBase b (div n b))++[(mod n b)]


-- Convert digits in base b to number
-- usage: fromBase b xs = n
-- assumes: b >= 0, each element of xs is >=0 and <b
-- assures: toBase b n = xs
fromBase :: Integer -> [Integer] -> Integer
fromBase b xs = let listIndex = [0..((length xs) - 1)]
                    exponentResult = map (b^) listIndex
                    multiplicativeResult = zipWith (*) (reverse xs) exponentResult
                    in foldl (+) (head multiplicativeResult) (tail multiplicativeResult)
