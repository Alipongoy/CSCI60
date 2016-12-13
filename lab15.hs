-- Lab 15: Partitions

-- Sets of Ints as lists. Invariant: sets as lists always has no duplicates
type Set = [Int]

-- Relations on sets of Ints. Invariant: no duplicates
type Rel = [(Int,Int)]

-- Universe (your code should work with any non-empty universe)
u = [1..8]

-- A partitition of u is a set of blocks, each of which is a set of elements
-- of u, which satisfies certain conditions (nontrivial, total, disjoint).
-- For example, here is the partitition of u corresponding to equivalence mod 3:
eqmod3_part :: [[Int]]
eqmod3_part = [[1,4,7], [2,5,8], [3,6]]


-- Write a function part that tests whether a list of lists is a partition of u,
-- including that each block is a set (i.e., has no duplicates)
part :: [[Int]] -> Bool
part bs = total bs bs
          where 
            nontrivial (b:bs) | (bs == []) = (b /= [])
                              | otherwise = if (b /= []) then nontrivial bs else False
            total (b:bs) xs | (bs == []) = [] 
                            | otherwise = iterate_through bs xs
                                          where
                                            iterate_through (c:cs) xs | (bs == []) = if (element c xs) then removeElement c xs
                                                                      | otherwise = if (element c xs) then ([c]++(iterate_through cs (removeElement c xs))) else (iterate_through cs xs)
            disjoint bs = and [ and [ (x == y) || (inters x y == []) | x <- bs] | y <- bs]


-- Write a function eq2part that takes an equivalence relation on u as input
-- and returns the associated partition of u. You can assume that the input is
-- really an equivalence relation on u.
-- eq2part :: Rel -> [[Int]]
-- eq2part rs = undefined


-- Write a function part2eq that takes a partition of u as input and returns
-- the associated equivalence relation on u. You can assume that the argument
-- is really a partition of u.
-- part2eq :: [[Int]] -> Rel
-- part2eq bs = undefined




noDups :: Eq a => [a] -> Bool
noDups (x:xs) | (xs == []) = True
              | otherwise = if (length (filter (\y -> y == x) xs) >= 1) then False else (noDups xs)

remDups :: Eq a => [a] -> [a]
remDups [] = []
remDups (x:xs) | (xs == []) = [x]
              | otherwise = if (length (filter (\y -> y == x) xs) >= 1) then remDups xs else x:(remDups xs)

-- elem finding function
element :: Eq a => a -> [a] -> Bool
element b (x:xs) | (xs == []) = (b == x)
                 | otherwise = if (b == x) then True else (element b xs)

-- Removes first element finding
removeElement :: Eq a => a -> [a] -> [a]
removeElement b (x:xs) | (xs == []) = if (b == x) then [] else [x]
                       | otherwise = if (b == x) then xs else x:(removeElement b xs)

-- inters :: Set -> Set -> Set
inters xs ys = let f' (x:xd) yd | (xd == [] && (element x yd) == True) = [x]
                                | (xd == [] && (element x yd) == False) = [ ]
                                | otherwise = if element x yd then x:(f' xd (tail yd)) else (f' xd yd)
                   in remDups (f' xs ys)

