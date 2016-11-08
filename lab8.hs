-- Joshua Jardenil
-- 12:00 PM
-- CSci 60 Lab 8

bools = [True, False]

-- The following code exhaustively checks whether not(P) \/ Q = P -> Q
-- (Try taking the "and" off this to see the list of 4 booleans)
not_equiv =and [(not p || q) == (p <= q) | p <- bools, q <- bools]


-- Write similar defintions that check each of the following tautologies:

-- P /\ Q = Q /\ P                           and is commutative
and_commutative = and [(p && q) == (q && p) | p <- bools, q <- bools]
-- P \/ Q = Q \/ P                           or is commutative
or_commutative = and [(p || q) == (q || p) | p <- bools, q <- bools]
-- P /\ (P -> Q) = P /\ Q
taut1 = and [(p && (p <= q) ) == (p && q) | p <- bools, q <- bools]
-- P -> (P -> Q) = P -> Q
taut2 = and [(p <= (p <= q) ) == (p <= q) | p <- bools, q <- bools]
-- P /\ (Q /\ R) = (P /\ Q) /\ R             and is associative
and_associative = and [(p && (q && r) ) == ((p && q) && r) | p <- bools, q <- bools, r <- bools]
-- P \/ (Q \/ R) = (P \/ Q) \/ R             or is associative
or_associative = and [(p || (q || r) ) == ((p || q) || r) | p <- bools, q <- bools, r <- bools]
-- P /\ (Q \/ R) = (P /\ Q) \/ (P /\ R)      and distributes over or
or_distributive = and [(p && (q || r) ) == ((p && q) || (p && r)) | p <- bools, q <- bools, r <- bools]
-- P \/ (Q /\ R) = (P \/ Q) /\ (P \/ R)      or distributes over and
and_distributive = and [(p || (q && r) ) == ((p || q) && (p || r)) | p <- bools, q <- bools, r <- bools]
-- P -> (Q /\ R) = (P -> Q) /\ (P -> R)      implies distributes over and
distributes_and_1 = and [(p <= (q && r) ) == ((p <= q) && (p <= r)) | p <- bools, q <- bools, r <- bools]
-- (P \/ Q) -> R = (P -> R) /\ (Q -> R)
distributes_and_2 = and [((p || q) <= r) == ((p <= r) && (q <= r)) | p <- bools, q <- bools, r <- bools]
-- P -> (Q -> R) = (P /\ Q) -> R
distributes_and_3 = and [(p <= (q <= r)) == ((p && q) <= r) | p <- bools, q <- bools, r <- bools]


-- The exclusive-or (xor) operation is equivalent to the /= operator in Haskell
-- Which of the following properties of exclusive-or are true:

-- xor is commutative
xor_commutative = and [(p /= q) == (q /= p) | p <- bools, q <- bools, r <- bools]
-- TRUE

-- xor is associative
xor_associative = and [(p /= (q /= r)) == ((p /= q) /= r) | p <- bools, q <- bools, r <- bools]
-- TRUE

-- xor distributes over and
xor_distributes_and = and [(p /= (q && r)) == ((p /= q) && (p /= r)) | p <- bools, q <- bools, r <- bools]
-- FALSE

-- xor distributes over or
xor_distributes_or = and [(p /= (q || r)) == ((p /= q) || (p /= r)) | p <- bools, q <- bools, r <- bools]
-- FALSE

-- and distributes over xor
and_distributes_xor = and [(p && (q /= r)) == ((p && q) /= (p && r)) | p <- bools, q <- bools, r <- bools]
-- TRUE

-- or distributes over xor
or_distributes_xor = and [(p || (q /= r)) == ((p || q) /= (p || r)) | p <- bools, q <- bools, r <- bools]
-- FALSE

-- implies distributes over xor
implies_distributes_xor = and [(p <= (q /= r)) == ((p <= q) /= (p <= r)) | p <- bools, q <- bools, r <- bools]
-- FALSE


-- Translate each of the statements below first, in a comment after "A: ",
-- into a logical statement involving forall, exists, /\, \/, ->, and not,
-- and then into Haskell code that checks ("brute force") whether
-- the statement is true. The universe of discourse in each case is [1..8]:

u = [1..8]

-- I'll work one example (which I actually did in class):

-- 1. "Every number that's greater than 2 is greater than 1"
-- A: forall n, (n > 2) -> (n > 1)
prob1 = and [(n > 2) <= (n > 1) | n <- u]

-- 2. Every number is either greater than 1 or less than 2
-- A:  forall n, (n > 1) \/ (n < 2)
prob2 = and [(n > 1) || (n < 2) | n <- u]

-- 3. Every two numbers are comparable with <= (i.e., either one is <=
--    the other or vice-versa)
-- A: forall n, forall m, (n -> m) \/ (m -> n) 
prob3 = and [(n <= m) || (m <= n) | n <- u, m <- u]

-- 4. For every odd number, there is a greater even number (use the Haskell
--    predicates odd, even :: Integral a => a -> Bool)
-- A: forall odd n,( thereis even m, (n < m) ) 
prob4 = and [ (or [n < m | m <- (filter (even) u)]) | n <- (filter (odd) u)]

-- 5. For every even number, there is a greater odd number
-- A: forall n, thereis m, m > n
prob5 = and [ (or [n < m | m <- (filter (odd) u)]) | n <- (filter (even) u)]

-- 6. There are two odd numbers that add up to 6
-- A: thereis n, (thereis m, (n + m = 6))
prob6 = or [(n + m == 6) | n <- (filter (\x -> odd x) u), m <- (filter (\x -> odd x) u) ]

-- 7. There is a largest number (i.e., there is a number that is >= all numbers)
-- A: thereis n, (forall m, n >= m)
prob7 =  or [ (or [n >= m | m <- u]) | n <- u]

-- 8. For every number, there is a different number such that there are no
--    numbers between these two.
-- A: forall n, (thereis m, n = m)
prob8 = and [ (or [(abs (m - n) == 1) | m <- u]) | n <- u]

prob9 = and [or [(x /= y) && (x > y) | y <- u] | x <- u]
