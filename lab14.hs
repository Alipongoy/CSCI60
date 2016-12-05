-- Joshua Jardenil
-- Tuesday: 12PM
-- Lab 14: Functions

-- Sets of Ints as lists. Invariant: sets as lists always has no duplicates
type Set = [Int]

-- A function F : A -> B, where A and B are finite sets of integers, can be
-- represented by giving the elements of A as a list, the elements of B as
-- a list, and the list of the ordered pairs in F:
type Funct = ([Int], [Int], [(Int,Int)])


-- Checks whether a Funct is functional, i.e., total and unique.
functional :: Funct -> Bool
functional (as,bs,rs) = or [elem (x,y) rs | x <- as, y <- bs] && and [b1 == b2 | a <- as, b1 <- bs, b2 <- bs, elem (a, b1) rs, elem (a, b2) rs]

-- Applies a function to an argument, i.e., given a function (as,bs,rs)
-- and an integer i, returns the unique j such that (i,j) is in rs.
-- You can assume that elem i as.
apply :: Funct -> Int -> Int
apply (as,bs,rs) i = [ b | (a,b) <- rs, (a == i)] !! 0

-- Checks if a function is one to one.
one_to_one :: Funct -> Bool
one_to_one (as,bs,rs) = and [ (length [ elem (x,y) rs | y <- bs, (elem (x,y) rs)] == 1) | x <- as]

-- Checks if a function is onto.
onto :: Funct -> Bool
-- For every B there exists an A
onto (as,bs,rs) = and [ or [elem (x,y) rs | x <- as] | y <- bs]

-- (Direct) image of a set under a function.
image :: Funct -> Set -> Set
image (as,bs,rs) xs = [apply (as,bs,rs) i | i <- xs, b <- bs, elem (i, b) rs]
  
-- Inverse image of a set under a function  
inverse_image :: Funct -> Set -> Set
inverse_image (as,bs,rs) ys = [x | i <- ys, x <- as, elem (x, i) rs]

-- Composition of two functions: (compose f g) takes x to f (g x)
-- Assumes that the two functions are composable
compose :: Funct -> Funct -> Funct
compose (as,bs,fs) (bs',cs,gs) = (as, cs, [(a,c) | a <- as, c <- cs, ((apply (bs',cs,gs) (apply (as,bs,fs) a)) == c)])

