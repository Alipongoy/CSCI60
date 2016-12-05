-- Joshua Jardenil
-- Lab 13
-- Tuesday 12:00 PM
---- Lab 12 ----

-- We will be working in the finite universe u = [1,2,3,4,5,6,7,8]
u = [1..8]


----- Relations on u

-- A relation, R, on U is a list of the ordered pairs of elements of U:
type Reln = [(Int,Int)]
              
-- For example, here are the < and <= relations
less_reln :: Reln
less_reln = [(i,j) | j <- [1..8], i <- [1..j-1]]

leq_reln :: Reln
leq_reln  = [(i,j) | j <- [1..8], i <- [1..j]]
            
-- and here is the relation of equivalence mod 3:
eqmod3_reln :: Reln
eqmod3_reln = [(i,j) | i <- [1..8], j <- [1..8], (j - i) `mod` 3 == 0]


-- Write a function refl that tests whether a relation is reflexive.
-- R is reflexive if: forall a, (a,a) in R
refl :: Reln -> Bool
refl rs = and [elem (z, z) rs | z <- u]

-- Write a function symm that tests whether a relation is symmetric:
-- R is symmetric if: forall a b, (a,b) in R -> (b,a) in R
symm :: Reln -> Bool
symm rs = and [elem (b,a) rs | (a,b) <- rs]

-- Write a function trans that tests whether a relation is transitive:
-- R is transitive if: forall a b c, (a,b) in R /\ (b,c) in R -> (a,c) in R
trans :: Reln -> Bool
trans rs = and [ elem (a,d) rs | (a,b) <- rs, (c,d) <- rs, (b == c)]

-- Check the <, <=, and eqmod3 relations for relexivity, symmetry, and
-- transitivity. Put the answers in a comment.

-- less_reln
  -- refl less_reln
    -- False
  -- symm less_reln
    -- False
  -- trans less_reln
    -- True

-- leq_reln
  -- refl leq_reln
    -- True
  -- symm leq_reln
    -- False
  -- trans leq_reln
    -- True

-- eqmod3_reln
  -- refl eqmod3_reln
    -- True
  -- symm eqmod3_reln
    -- True
  -- trans eqmod3_reln
    -- True

-- For each of the 8 possible combinations of yes/no on reflexivity,
-- symmetry, and transitivity, find a MINIMAL relation on u that has exactly
-- that combination of properties. Add a test to check whether you got the
-- properties right. (I'll do the first one as an example, and give you the
-- test for the second one.)

-- refl, symm, trans
rst :: Reln
rst = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8)]
rst_test = refl rst && symm rst && trans rst

-- refl, symm, not trans
rst' :: Reln
rst' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2), (2,1), (2,3), (3,2)]
rst'_test = refl rst' && symm rst' && not (trans rst')

-- refl, not symm, trans
rs't :: Reln
rs't = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2), (2,3), (1,3)]
rs't_test = refl rs't && not (symm rs't) && trans rs't

-- refl, not symm, not trans
rs't' :: Reln
rs't' = [(1,1), (2,2), (3,3), (4,4), (5,5), (6,6), (7,7), (8,8), (1,2), (2,3)]
rs't'_test = refl rs't' && not (symm rs't') && not (trans rs't')

-- not refl, symm, trans
r'st :: Reln
r'st = []
r'st_test = not (refl r'st) && symm r'st && trans r'st

-- not refl, symm, not trans
r'st' :: Reln
r'st' = [(1,5), (5,1)]
r'st'_test = not (refl r'st') && symm r'st' && not (trans r'st')

-- not refl, not symm, trans
r's't :: Reln
r's't = [(1,2), (2,3), (1,3)]
r's't_test = not (refl r's't) && not (symm r's't) && trans r's't

-- not refl, not symm, not trans
r's't' :: Reln
r's't' = [(1,2), (2,3), (1,5)]
r's't'_test = not (refl r's't') && not (symm r's't') && not (trans r's't')
