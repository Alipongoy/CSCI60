-- Joshua Jardenil
-- 12:00 PM

-- CSCI 60
-- Lab 3

---------------PART 1----------------------------
-- Read Chapter 5 - 6 of the tutorial

-- Using what you learned from Chapter 5:
-- Recursively define the length of a list

length' :: [Integer] -> Integer
length' [] = 0
length' (x:xs) | length xs == 0 = 1
               | otherwise = 1 + length' xs 

-- Reverse a list recursively
reverse' :: [Integer] -> [Integer]
reverse' [] = []
reverse' (x:xs) | length xs == 0 = [x]
                | otherwise = reverse' xs ++ [x]

-- Find the second to last element of a list
sndToLast :: [Integer] -> Integer
sndToLast xs | length xs == 2 = head xs
             | otherwise = sndToLast(tail xs)

---------------PART 2----------------------------

-- Definition. Two numbers a and b are coprime if gcd(a,b) = 1.
-- Give a definition of this check:

coprime :: Integer -> Integer -> Bool
-- Returns true if a == 1 false otherwise
coprime a b | (b == 0) = (a == 1)
            | otherwise = coprime b (rem a b)

-- Extend this to a function on lists:
-- coprime_with n list = True exactly when n is coprime with
-- every element of list.

coprimeWith :: Integer -> [Integer] -> Bool
coprimeWith a xs | coprime a (head xs) == False = False  
-- Detecting last element
                 | (coprime a (head xs) == True) && (head xs == (xs !! (length xs - 1))) = True
                 | otherwise = coprimeWith a (tail xs)
 
-- Pairwise coprime.  This is true of a list if every pair of
-- numbers in the list are coprime

pairwiseCoprime :: [Integer] -> Bool
pairwiseCoprime xs | coprimeWith (head xs) (tail xs) == False = False
                    | (coprimeWith (head xs) (tail xs) == True) && (length xs == 2) = True
                    | otherwise = pairwiseCoprime (tail xs)