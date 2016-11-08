-- Joshua Jardenil
-- 12:00 PM
-- Lab 6: RSA

import System.Random
import System.IO.Unsafe

---- Given functions ----------------------------------------------------------

-- Functions from earlier labs
extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (a, 1, 0)
extGCD a b = let (q, r) = divMod a b
                 (d, m, n) = extGCD b r
             in (d, n, m - n*q)

minv :: Integer -> Integer -> Integer
minv a m = let (1, ainv, _) = extGCD a m
           in ainv `mod` m

toBase :: Int -> Integer -> [Int]
toBase b n = reverse (toBase' b n) where
  toBase' b 0 = []
  toBase' b n = let (q,r) = divMod n (fromIntegral b)
                in (fromIntegral r) : toBase' b q

fromBase :: Int -> [Int] -> Integer
fromBase b ds = fromBase' b (reverse ds) where
  fromBase' b [] = 0
  fromBase' b (d:ds) = (fromIntegral d) + (fromIntegral b) * fromBase' b ds


-- Miller-Rabin primality test (made deterministic)
isPrime :: Integer -> Bool
isPrime n = unsafePerformIO (isMillerRabinPrime 100 n)
 
isMillerRabinPrime :: Int -> Integer -> IO Bool
isMillerRabinPrime k n
   | even n    = return (n==2)
   | n < 100   = return (n `elem` primesTo100)
   | otherwise = do ws <- witnesses k n
                    return $ and [test n (pred n) evens (head odds) a | a <- ws]
  where
    (evens,odds) = span even (iterate (`div` 2) (pred n))
 
test :: Integral nat => nat -> nat -> [nat] -> nat -> nat -> Bool
test n n_1 evens d a = x `elem` [1,n_1] || n_1 `elem` powers 
  where
    x = powerMod n a d
    powers = map (powerMod n a) evens
 
witnesses :: (Num a, Ord a, Random a) => Int -> a -> IO [a]
witnesses k n 
  | n < 9080191         = return [31,73]
  | n < 4759123141      = return [2,7,61]
  | n < 3474749660383   = return [2,3,5,7,11,13]
  | n < 341550071728321 = return [2,3,5,7,11,13,17]
  | otherwise           = do g <- newStdGen
                             return $ take k (randomRs (2,n-1) g)
 
primesTo100 :: [Integer]
primesTo100 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
 
-- powerMod m x n = x^n `mod` m (efficient version, changed order of arguments)
powerMod :: Integral nat => nat -> nat -> nat -> nat
powerMod m x n  = f (n - 1) x x `rem` m 
  where
  f d a y = if d==0 then y else g d a y 
  g i b y | even i    = g (i `quot` 2) (b*b `rem` m) y
          | otherwise = f (i-1) b (b*y `rem` m)


-- Allowable characters in messages (you can try other sets of characters).
message_chars = ['a'..'z'] ++ ['0'..'9'] ++ " .,!?"

-- Find the index of a character in a list of characters (like message_chars)
-- Assumes that the character actually occurs in the list.
elemIndex :: Char -> [Char] -> Int
elemIndex c (x:xs) | c == x = 0
                   | otherwise = 1 + elemIndex c xs

intElemIndex :: Int -> [Int] -> Int
intElemIndex c (x:xs) | c == x = 0
                   | otherwise = 1 + intElemIndex c xs

-- blocks xs sz = result of dividing xs up into a list of strings of size sz
blocks :: [Char] -> Int -> [[Char]]
blocks w n | null w2 = [w1]
           | otherwise = w1 : blocks w2 n
   where (w1, w2) = splitAt n w


---- Lab 6 starts here --------------------------------------------------------

-- nextPrime n returns the least prime number >= n
nextPrime :: Integer -> Integer
nextPrime n = head $ dropWhile (\x -> not (isPrime x) || x == n) [n..]

-- nextCoprime n m returns the least number >= n that is coprime to m
nextCoprime :: Integer -> Integer -> Integer
nextCoprime n m = let (d, _, _) = extGCD n m 
                  in (nextCoprime' n d) where
                    nextCoprime' n d | (d == 1) = n
                                     | otherwise = nextCoprime (nextPrime n) d

-- chooseKeys p q e = (e', d') where e' is the smallest integer >= e that has
-- a multiplicative inverse mod N = (p-1)*(q-1), and d' is that inverse.
-- You can assume that p and q are prime.
chooseKeys :: Integer -> Integer -> Integer -> (Integer,Integer)
chooseKeys p q e = let n = (p-1)*(q-1)
                       e' = nextCoprime e n
                       d' = minv e' n
                   in (e', d')


-- crypt (m, key) [m1,...,mk] = [c1,...,ck], where m = p*q, key is either
-- e or d, and where the mi' are the results of en- or de-crypting the mi.
crypt :: (Integer, Integer) -> [Integer] -> [Integer]
crypt (m, key) ms | ((length ms) == 0) = []
                  | otherwise = (powerMod m (head ms) key):(crypt (m,key) (tail ms))

-- blockSize m b = the number of base-b digits that can be represented by
-- an Integer < m. Hint: calculate this by repeatedly multiplying 1 by b
-- until you get >= m, returning one less than the number of multiplications
blockSize :: Integer -> Int -> Int
blockSize m b = let index = 0
                in (blockSize' m b index) where
                  blockSize' m b index | (fromIntegral m < (b ^ index)) = index
                                       | otherwise = blockSize' m b (index + 1)

-- str2Integer xs = the number resulting from treating the string xs, whose
-- characters are all assumed to come from message_chars, as a number in
-- base b, where b = length message_chars (i.e., the characters of xs are
-- themselves the digits of the number in base b)
str2Integer :: [Char] -> Integer
str2Integer xs = (str2Integer' xs) - 1 where
  str2Integer' xs = let charIndex = map (\x -> (elemIndex x message_chars) + 1) xs
                        rangeIndex = reverse [0..(length xs - 1)] 
                        expIndex = map (\x -> (length message_chars)^x) rangeIndex 
                        multiplicativeResult = zipWith (\x y -> x*y) charIndex expIndex
                     in toInteger (foldl (+) (head multiplicativeResult) (tail multiplicativeResult))
  
 
-- the inverse function that converts the Integer back into a string
-- integer2str :: Integer -> [Char]
integer2str n | blockSize n (length message_chars) <= 1 = [message_chars !! (fromIntegral n)] 
              | otherwise = let rangeIndex = [1..(length message_chars)]
                                bSize = blockSize n (length message_chars) - 1
                                multiplicativeResult = zipWith (\x y -> x*y) (map (\x -> (length message_chars) ^ bSize) rangeIndex) rangeIndex
                                divNum = last (takeWhile (\x -> ((fromIntegral n) `div` x) > 0) multiplicativeResult)
                                divChar = intElemIndex divNum multiplicativeResult
                            in (message_chars !! (fromIntegral divChar)):(integer2str (n - (toInteger divNum)))


-- mkMessage xs m = [m1,...,mk], where xs is the message, m = p * q, and
-- m1,...,mk are the Integers representing the chunks of the message
mkMessage :: [Char] -> Integer -> [Integer]
mkMessage xs m | ((length xs) == 0) = []
               | otherwise = let message z m = if ((str2Integer z) < m) then z else (message (init z) m)
                             in (str2Integer (message xs m)):(mkMessage(drop (length (message xs m)) xs) m)
            

{- Now use the above functions to
   1. choose two large prime numbers
   2. choose keys e and d
   3. convert a longer message into a list of Integers
   4. encrypt those Integers using crypt (m,e)
and then
   5. decrypt those Integers using crypt (m,d)
   6. convert the Integers back into strings
   7. concatenate the strings to get the original message back
-}
p = 11113
q = 22229
m = p * q
n = (p-1)*(q-1)
e = nextCoprime 5 n
(e', d) = chooseKeys p q e
message = "obama"
brokenMessage = mkMessage message m
enc = crypt (m, e) brokenMessage
dec = crypt (m, d) enc
decryptedMessage = map (\x -> integer2str x) dec

con z | (length z == 0) = []
      | otherwise = (head z):(con (tail z))

concatenatedMessage = head (con decryptedMessage)



