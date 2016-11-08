-- Lab 7: Review and tracing

import Debug.Trace (trace)

{- The type and specification of trace are

    -- trace xs e = e, but prints out xs as a side-effect
    trace :: String -> a -> a

The way you can use trace to get a trace of calls to your function is to:
  * add a new clause to your function before all of the others
  * the arguments to your function should all be variables (or underscores)
  * the clause should have a guard of the form "trace ... False"
  * the ... should be a string containing tracing information (using show)
  * the value for this clause should be undefined.

If you also want to trace the values that are returned from the function,
replace your return value, e, with let ans = e in trace (show ans) ans.

-}

-- Tracing version of the Division Algorithm
divAlg :: Integer -> Integer -> (Integer, Integer)
divAlg a b | trace (show (a,b)) False = undefined
divAlg a b | a < b = let ans = (0,a) in trace ("--> " ++ show ans) ans
           | otherwise = let (q,r) = divAlg (a-b) b
                             ans = (q+1,r)
                         in trace ("--> " ++ show ans) ans

{- Output of divAlg 28 5:

(28,5)
(23,5)
(18,5)
(13,5)
(8,5)
(3,5)
--> (0,3)
--> (1,3)
--> (2,3)
--> (3,3)
--> (4,3)
--> (5,3)
(5,3)

-}


-- For each of the functions below, create a tracing version and then put
-- into a comment the output of a typical call, as I did above.


-- Tracing version of Euclidean algorithm from Lab 2
euclid :: Integer -> Integer -> Integer
euclid a b | trace (show (a,b)) False = undefined
           | b == 0 = let ans = a in trace ("--> " ++ show ans) ans 
           | otherwise = let r = mod a b 
                         in (trace ("--> " ++ show r) euclid b r)

{- Output for euclid 28 5 
(28,5)
--> 3
(5,3)
--> 2
(3,2)
--> 1
(2,1)
--> 0
(1,0)
--> 1
1
-}


-- Tracing version of coprimeWith and pairwiseCoprime from Lab 3
coprimeWith :: Integer -> [Integer] -> Bool
coprimeWith a xs | trace (show (a,xs)) False = undefined
                 | (gcd a (head xs) == 1) == False = let ans = False in trace("--> " ++ show ans) ans   
                 | (gcd a (head xs ) == 1) == True && (head xs == (xs !! (length xs - 1))) = let ans = True in trace ("--> " ++ show ans) ans
                 | otherwise = let ans = True in (trace ("--> " ++ show ans) coprimeWith a (tail xs))

{- Output for coprimeWith 5 [6,7,9,10]
(5,[6,7,9,10])
--> True
(5,[7,9,10])
--> True
(5,[9,10])
--> True
(5,[10])
--> False
False
-}


-- pairwiseCoprime :: [Integer] -> Bool
-- pairwiseCoprime xs = let list1 = xs
--                      in 

 

-- Tracing version of Extended GCD
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a b | trace (show(a,b)) False = undefined
           | b == 0 = let ans = (a, 1, 0) in trace ("--> " ++ show ans) ans
           | otherwise = let (d, m, n) = extGCD b (rem a b)
                             (q, r) = quotRem a b
                             ans = (d,m,n)
                         in trace ("--> " ++ show ans) ans

{- Output for extGCD 28 5
(28,5)
(5,3)
(3,2)
(2,1)
(1,0)
--> (1,1,0)
--> (1,0,1)
--> (1,1,-1)
--> (1,-1,2)
--> (1,2,-11)
(1,2,-11)
-}



-- Tracing version of Prime Factorization
primeFactsWith :: Integer -> [Integer] -> [Integer]
primeFactsWith n (p:ps) | trace (show(n) ++ " " ++ show(p)) False = undefined
primeFactsWith n (p:ps) | ((length ps == 0) && (n `mod` p /= 0)) = trace("--> " ++ show False) []
                        | ((length ps == 0) && (n `mod` p == 0)) = trace("--> " ++ show True) [p]
                        | (n `mod` p == 0) = trace("--> " ++ show True) (p:(primeFactsWith (div n p) (p:ps)))
                        | (n `mod` p /= 0) = trace("--> " ++ show False) (primeFactsWith n ps)

-- Tracing versions of toBase and fromBase (trace the helper functions)
toBase :: Integer -> Integer -> [Integer]
toBase b n = reverse (toBase' b n) where
  toBase' b 0 = []
  toBase' b n = let (q,r) = divMod n (fromIntegral b)
                    ans = (q,r)
                in trace (show ans) (fromIntegral r) : toBase' b q


fromBase :: Integer -> [Integer] -> Integer
fromBase b xs = fromBase' b (reverse xs) where
  fromBase' b [] = 0
  fromBase' b (d:ds) = trace ("--> " ++ show(b, d)) (fromIntegral d) + (fromIntegral b) * fromBase' b ds


