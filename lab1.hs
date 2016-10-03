-- CSci 60 Lab 1

---------------- Part 1 ----------------

-- Work through Chapters 1 - 3 of Learn You a Haskell for Great Good!
-- Type in the examples and make sure you understand the results.
-- Ask questions about anything you don't understand! This is your
-- chance to get off to a good start understanding Haskell.


---------------- Part 2 ----------------

-- The Haskell Prelude has a lot of useful built-in functions related
-- to numbers and lists.  In Part 2 of this lab, you will catalog many
-- of these functions.

data Color = Red | Orange | Yellow | Green | Blue | Violet
     deriving (Show, Eq, Ord, Enum)

-- For each of the Prelude functions listed below, give its type,
-- describe in your own words what the function does, answer any
-- questions specified, and give several examples of its use.  Does
-- the function apply at all to the "Color" type defined above?


-- succ, pred
--
-- succ :: Enum a => a -> a
-- 1) Succ is of type a and returns a type a
-- 2) Succ returns the next element of a number or character or sequence.
-- Examples) succ 7 -> 8, succ 'I' -> J, succ Red -> Orange
-- 3) This function does apply to the "Color" types
--
-- pred :: Enum a => a -> a
-- 1) pred is of type a and returns a type a
-- 2) pred returns the previous element of a number, character, or sequence.
-- Examples) pred 7 -> 6, pred 'I' -> 'H', pred Violet -> Blue
-- 3) This function does apply to the "Color" types

-- toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo
--
-- toEnum :: Enum a => Int -> a
-- 1) toEnum is of type "a" that takes an Int as an argument
-- 2) toEnum converts an Int to a type Enum.
-- Examples) toEnum 0 :: Color -> Red, toEnum 57 :: Char -> '9'
-- 3) This function does apply to the "Color" types
--
-- fromEnum :: Enum a => a -> Int
-- 1) fromEnum is of type a and returns an Int
-- 2) fromEnum converts an Enum to an Int.
-- Examples) fromEnum Red -> 0, fromEnum '9' -> 57
-- 3) This function does apply to the "Color" types
--
-- enumFrom :: Enum a => a -> [a]
-- 1) enumFrom is of type "a" and returns a list of type "a"
-- 2) enumFrom accepts a generic type and returns a list of what is included under that type.
-- Examples) enumFrom 6 -> [1..], enumFrom Red -> [Red,Orange,Yellow,Green,Blue,Violet]
-- 3) This function does apply to the "Color" types
--
-- enumFromThen :: Enum a => a -> a -> [a]
-- 1) enumFromThen is of type "a" and returns a list of type "a"
-- 2) enumFromThen takes two arguments of the same type and returns a patterned list, depending on how it was patterned.
-- Examples) enumFromThen 1 6 -> [1, 6, 11, 16..], enumFromThen Red Orange -> [Red, Orange, Yellow, Green, Blue, Violet]
-- 3) This function does apply to the "Color" types
--
-- enumFromTo :: Enum a => a -> a -> [a]
-- 1) enumFromTo is of type "a" and returns a list of type "a"
-- 2) enumFromTo accepts two arguments and creates a list from argument1 to argument2.
-- Examples) enumFromTo Red Green -> [Red,Orange,Yellow,Green], enumFromTo 6 10 -> [6,7,8,9,10]
-- 3) This function does apply to the "Color" types
--
-- enumFromThenTo :: Enum a => a -> a -> a -> [a]
-- 1) enumFromThenTo is of type "a" and returns a list of type "a"
-- 2) enumFromThenTo accepts three arguments. The first argument is the start point, the second argument determines the pattern, and the last argument determines the end point.
-- Examples) enumFromThenTo 6 10 18 -> [6,10,14,18], enumFromThenTo Red Orange Blue -> [Red,Orange,Yellow,Green,Blue]
-- 3) This function applies to all "Color" types

-- ==, /=
-- (==) :: Eq a => a -> a -> Bool)
-- 1) (==) is of type "a" and returns a Bool.
-- 2) (==) accepts two arguments and compares the two arguments. If they are equal, True is returned. If they are not, False is returned.
-- Examples) Red == Red -> True, 6 == 7 -> False
-- 3) This function applies to all "Color" types
--
-- (/=) :: Eq a => a -> a -> Bool)
-- 1) (/=) is of type "a" and returns a Bool.
-- 2) (/=) accepts and compares two arguments. If these two arugments aren't equal, True is returned. Else, False is returned.
-- Examples) 6 /= 7 -> True, 6 /= 6 -> False, Red /= Yellow -> True
-- 3) This function applies to all "Color" types

-- quot, div            (Q: what is the difference? Hint: negative numbers)
--
-- quot :: Integral a => a -> a -> a
-- 1) quot is of type "a", takes 2 arguments, and returns a type "a"
-- 2) quot returns the division of the first argument with the second argument. If any of the arguments are negative, then the result is rounded up.
-- Examples) quot 7 6 -> 1, quot (-7) 6 -> -1
-- 3) This function does NOT apply to "Color" types
--
-- div :: Integral a => a -> a -> a
-- 1) div is of type "a", takes two arguments, and returns type "a"
-- 2) div returns the division of the first argument with the second argument. Unlike quot, div rounds down if the result is negative.
-- Examples) div 7 6 -> 1, div -7 6 -> -2
-- 3) This function does NOT apply to "Color" types

-- rem, mod             (Q: what is the difference? Hint: negative numbers)
--
-- rem :: Integral a => a -> a -> a
-- 1) rem is of type "a", takes two arguments, and returns a type "a"
-- 2) rem returns the remainder of the division between the second argument and the first argument. If the result of the division is negative, then the remainder is also negative.
-- Examples) rem 7 6 -> 1, rem -7 6 -> -1
-- 3) This function does NOT apply to "Color" types
--
-- mod :: Integral a => a -> a -> a
-- 1) mod is of type "a", takes two arguments, and retuns a type "a"
-- 2) mod returns the positive remainder of the division between the first argument with the second argument. If the remainder is negative, then the divisor is added to the remainder.
-- Examples) mod 7 6 -> 1, mod -7 6 -> 5
-- 3) This function does NOT apply to "Color" types

-- quotRem, divMod
--
-- quotRem :: Integral a => a -> a -> (a, a)
-- 1) quotrem is of type "a", takes two arguments, and returns a tuple of type "a"
-- 2) quotRem returns a tuple consisting of the result of division with the first argument with the second argument. The first element of the tuple is the quotient and the second element of the tuple is the remainder. If the result is negative, the contents of the tuple are both negative as well.
-- Examples) quotRem 7 6 -> (1, 1), quotRem (-7) 6 -> (-1, -1)
-- 3) this function does NOT apply to "Color" types
--
-- divMod :: Integral a => a -> a -> (a, a)
-- 1) quotrem is of type "a", takes two arguments, and returns a tuple of type "a"
-- 2) quotRem returns a tuple consisting of the result of division with the first argument with the second argument. The first element of the tuple is the quotient and the second element of the tuple is the remainder. If the result is negative, the contents of the tuple are positive.
-- 3) this function does NOT apply to "Color" types

-- &&, ||
--
-- (&&) :: Bool -> Bool -> Bool
-- 1) (&&) is of type Bool, takes two Bool arguments, and returns a Bool
-- 2) (&&) returns True if and only if both arguments are true.
-- Examples) True && True -> True, True && False -> False, False && False -> False
-- 3) this function does NOT apply to "Color" types
--
-- (||) :: Bool -> Bool -> Bool)
-- 1) (||) is of type Bool, takes two Bool arguments, and returns a Bool
-- 2) (||) returns True if at least of the arguments are true, false otherwise.
-- Examples) True || False -> True, True || True -> True, False || False -> False
-- 3) this function does NOT apply to "Color" types

-- ++
-- 
-- (++) :: [a] -> [a] -> [a]
-- 1) (++) is of type "a", requires two arguments of lists of type "a", and returns a list of type "a"
-- 2) (++) returns the addition of list1 with list2.
-- Examples) [1,2,3] ++ [4,5,6] -> [1,2,3,4,5,6], "Hello " ++ "World" -> "Hello World", [Red, Blue] ++ [Green, Red] -> [Red,Blue,Green,Red]
-- 3) this function does apply to "Color" typees

-- compare
-- compare :: Ord a => a -> a -> Ordering
-- 1) compare is of type "a", takes two arguments, and returns and Ordering
-- 2) compare returns LT, GT, or EQ, depending if the first argument is less than, greater than, or equal to the second argument.
-- Examples) compare Red Violet -> LT, compare 6 6 -> EQ, compare 'z' 'a' -> GT
-- 3) this function does apply to "Color" typees

-- <, >
--
-- (<) :: Ord a => a -> a -> Bool
-- 1) (<) is of type "a", takes two arguments, and returns a Bool
-- 2) (<) returns True if the first argument is less than the second argument and false otherwise.
-- Examples) 6 < 7 -> True, Red < Orange -> True, 69 < 42 -> False
-- 3) this function does apply to "Color" typees
--
-- (>) :: Ord a => a -> a -> Bool
-- 1) (>) is of type "a", takes two arguments, and returns a Bool
-- 2) (>) returns True if the first argument is greater than the second argument and false otherwise.
-- Examples) 6 > 7 -> False, Red > Orange -> False, 420 > 1 -> True
-- 3) this function does apply to "Color" typees

-- max, min
--
-- max :: Ord a => a -> a -> a
-- 1) max is of type "a", takes two arguments, and returns a type "a"
-- 2) max returns the greater of the two arguments.
-- Examples) max 7 6 -> 7, max Blue Red -> Blue
-- 3) this function does apply to "Color" typees
--
-- min :: Ord a => a -> a -> a
-- 1) min is of type "a", takes two arguments, and returns a type "a"
-- 2) min returns the lesser of the two arguments.
-- Examples) min 7 6 -> 7, min Blue Red -> Blue
-- 3) this function does apply to "Color" typees

-- ^
-- (^) :: (Integral b, Num a) => a -> b -> a
-- 1) (^) is of type "a" and "b", takes two arguments, and returns a Num of type "a"
-- 2) (^) returns the first argument sqaured the second argument amount of times.
-- Examples) 7 ^ 2 -> 49, (-1) ^ 1 -> -1
-- 3) this function does NOT apply to "Color" types

-- all, any
--
-- all :: Foldable t => (a -> Bool) -> t a -> Bool
-- 1) all is of type "t" and "a", takes two arguments, and returns a Bool.
-- 2) all returns true if the first argument evaluated is true for all elements of the list.
-- Examples) all ('A'==) "AAA" -> True, (5<) [1,2,7] -> False, all (Red==) [Red, Red, Red] -> True
-- 3) This function does apply to "Color" types
--
-- any :: Foldable t => (a -> Bool) -> t a -> Bool
-- 1) any is of type "t" and "a", takes two arguments, and returns a Bool.
-- 2) any returns true if the first argument evaluated is true for at lease on element of the list.
-- Examples) all ('A'==) "AAA" -> True, (5<) [1,2,7] -> True, all (Red==) [Red, Green, Blue] -> True
-- 3) This function does apply to "Color" types

-- break
--
-- break :: (a -> Bool) -> [a] -> ([a], [a])
-- 1) break is of type "a", accepts two arguments, and returns a tupled list of type "a"
-- 2) break breaks the list into a tupled list from the first element to the element where the first arugment is evaluated true.
-- Examples) break (1==) [1,2,3,4,1,5] -> ([1], [2,3,4,1,5)), break (Green<) [Orange, Red, Blue] -> ([Orange, Red], [Blue])
-- 3) This function does apply to "Color" types

-- concat
--
-- concat :: Foldable t => t [a] -> [a]
-- 1) concat is of type "t" and "a", accepts one argument, and returns a list of type "a"
-- 2) concat combines all the lists inside of a list into one list.
-- Examples) concat [[1,2,3],[4,5,6]] -> [1,2,3,4,5,6], concat [[Red], [Blue, Green]] -> [Red, Blue, Green]
-- 3) This function does apply to "Color" types

-- const
--
-- const :: a -> b -> a
-- 1) const is of type "a" and "b", takes two arguments, and returns a type "a"
-- 2) const returns the first argument.
-- Examples) const 4 'A' -> 4, const Red Blue -> Red
-- 3) This function does apply to "Color" types

-- cycle
--
-- cycle :: [a] -> [a]
-- 1) cycle is of type "a", accepts one argument, and returns a list of type "a"
-- 2) cycle creates an infinite list of the first argument.
-- Examples) cycle [5] -> [5..], cycle [Red, Blue] -> [Red, Blue..]
-- 3) This function does apply to "Color" types

-- drop, take
-- 
-- drop :: Int -> [a] -> [a]
-- 1) drop is of type "a", accepts two arguments, and returns a lsit of type "a"
-- 2) drop drops the front of the list the first argument amount of times. The remainder gets returned.
-- Examples) drop 5 [1,2,3,4,5,6] -> [6], drop 3 (cycle [Red, Blue]) -> [Red, Blue, Red]
-- 3) This function does apply to "Color" types
--
-- take :: Int -> [a] -> [a]
-- 1) take is of type "a", accepts two arguments, and returns a lsit of type "a"
-- 2) take keeps the first argument amount of elements, starting from the front. 
-- Examples) take 5 [1,2,3,4,5,6] -> [1,2,3,4,5], take 3 (cycle [Red, Blue]) -> [Red, Blue, Red]
-- 3) This function does apply to "Color" types

-- dropWhile, takeWhile
--
-- dropWhile :: (a -> Bool) -> [a] -> [a]
-- 1) dropWhile is of type "a", takes two arguments, and returns a list of type "a"
-- 2) dropWhile drops the front of the second argument list until the first argument is False. The remainder is returned.
-- Examples) dropWhile (<3) [1,2,3,4,5] -> [3,4,5], dropWhile (=='F') "Foo" -> "oo", dropWhile (==Red) [Blue, Green, Red] -> [Blue, Green, Red]
-- 3) This function does apply to "Color" types
--
-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- 1) takeWhile is of type "a", takes two arguments, and returns a list of type "a"
-- 2) takeWhile keeps the front of the second argument list until the first argument is False. 
-- Examples) takeWhile (<3) [1,2,3,4,5] -> [1,2], dropWhile (=='F') "Foo" -> "F", dropWhile (==Red) [Blue, Green, Red] -> [Blue, Green]
-- 3) This function does apply to "Color" types

-- elem
--
-- elem :: (Eq a, Foldable t) => a -> t a -> Bool
-- 1) elem is of type "t" and "a", accepts two arguments, and returns a Bool
-- 2) elem returns False if there is no case of the first argument in the list of second arguments. If there is a case, True is returned.
-- Examples) elem 6 [1,2,3] -> False, elem Red [Red, Blue, Green] -> True
-- 3) This function does apply to "Color" types
--
-- even
--
-- even :: Integral a => a -> Bool
-- 1) even is of type "a", accepts one argument, and returns a Bool
-- 2) even returns True if the first argument is even, False otherwise.
-- Examples) even 9 -> False, even 8 -> True
-- 3) This function does NOT apply to "Color" types

-- filter
--
-- filter :: (a -> Bool) -> [a] -> [a]
-- 1) filter is of type "a", accepts two arguments, and returns a list of type "a"
-- 2) filter selects the elements of the second argument list where the first argument is true.
-- Examples) filter (/=5) [1,5,5,2,5,3] -> [1,2,3], filter (==Red)  [Red, Blue, Red] -> [Red, Red]
-- 3) This function does apply to "Color" types

-- fst
--
-- fst :: (a, b) -> a
-- 1) fst is of type "a" and "b", accepts one argument, and returns a type "a"
-- 2) fst returns the first element of a tuple.
-- Examples) fst (4,5) -> 4, fst (Red, Blue) -> Red
-- 3) This function does apply to "Color" types

-- gcd
--
-- gcd :: Integral a => a -> a -> a
-- 1) gcd is of type "a", accepts two arguments, and returns a type "a"
-- 2) gcd returns the greatest common denominator of the two arguments.
-- Examples) gcd 2 10 -> 2, gcd 5 7 -> 5
-- 3) This function does NOT apply to "Color" types

-- head
--
-- head :: [a] -> a
-- 1) head is of type "a", accepts a list of type "a", and returns a type "a"
-- 2) head returns the first element of the list argument.
-- Examples) head [Red, Blue, Green] -> Red, head [5,6,7] -> 5
-- 3) This function does apply to "Color" types

-- id
--
-- id :: a -> a
-- 1) id is of type "a", accepts one argument, and returns a type "a"
-- 2) id returns itself.
-- Examples) id 7 -> 7, id Red -> Red
-- 3) This function does apply to "Color" types

-- init
--
-- init :: [a] -> [a]
-- 1) init is of type "a", accepts a list of type "a", and returns a list of type "a"
-- 2) init returns every element but the last list.
-- Examples) init [1,2,3] -> [1,2], init [Red, Green, Yellow] -> [Red, Green]
-- 3) This function does apply to "Color" types

-- iterate
--
-- iterate :: (a -> a) -> a -> [a]
-- 1) iterate is of type "a", accepts two arguments, and returns a list of type "a"
-- 2) iterate applies the the result of the first argument to the second argument and appends the result to a list infinitely.
-- Examples) iterate (5+) 1 -> [1,6,11..] -> iterate ([Red]++) [Red] -> [[Red], [Red, Red], [Red, Red, Red]...]
-- 3) this function does apply to "color" types

-- last
-- 
-- last :: [a] -> a
-- 1) last is of type "a", takes a list of type "a", and returns an element of type "a"
-- 2) last returns the last element of a list given.
-- Examples) last [1,2,3] -> 3, last [Red, Blue, Green] -> Green
-- 3) this function does apply to "color" types

-- lcm
--
-- lcm :: Integral a => a -> a -> a
-- 1) lcm is of type "a", accepts two arguments of type "a", and returns a type "a"
-- 2) lcm returns the least common multiple between the two given arguments.
-- Examples) lcm 2 3 -> 6, lcm 26 5 -> 130
-- 3) this function does NOT apply to "color" types

-- length
--
-- length :: Foldable t => t a -> Int
-- 1) length is of type "a", accepts a foldable list of type "a"'s, and returns an Int.
-- 2) length returns an Int consisting of the list length.
-- Examples) length [1,2,3] -> 3, length [Red, Red, Blue] -> 3
-- 3) this function does apply to "color" types

-- map
--
-- map :: (a -> b) -> [a] -> [b]
-- 1) map is of type "a" and "b", accepts a list of type "a"s, and returns a list of type "b"s.
-- 2) map takes the first argument and applies the evaluation between the first argument and each element of the list. A newlist is returned.
-- Examples) map (5+) [10, 20, 30] -> [15, 25, 35], map (Red==) [Red, Yellow, Violet] -> [True, False, False]
-- 3) this function does apply to "color" types

-- null
--
-- null :: Foldable t => t a -> Bool
-- 1) null is of type "t" and "a", accepts one argument, and returns a Bool.
-- 2) null returns True if a list is empty and False if the list isn't empty.
-- Examples) null [] -> True. null [Red] -> False
-- 3) this function does apply to "color" types

-- odd
--
-- odd :: Integral a => a -> Bool
-- 1) odd is of type "a", accepts one argument, and returns a Bool
-- 2) odd returns True if the first argument is odd, False otherwise.
-- Examples) odd 3 -> True, odd 4 -> False
-- 3) this function does NOT apply to "color" types

-- repeat
-- 
-- repeat :: a -> [a]
-- 1) repeat is of type "a", accept one argument, and returns a list of type "a"
-- 2) repeat returns an infinite list consisting of the repitition of the first argument.
-- Examples) repeat 5 -> [5..], repeat [Blue, Violet] -> [[Blue, Violet],[Blue, Violet]..]
-- 3) this function does apply to "color" types

-- replicate
--
-- replicate :: Int -> a -> [a]
-- 1) replicate is of type "a", accept two arguments, and returns a list of type "a"
-- 2) replicate returns the a list of the second argument replicated the first argument amount of times.
-- Examples) replicate 5 Red -> [Red, Red, Red, Red, Red], replicate 3 1 -> [1,1,1]
-- 3) this function does apply to "color" types

-- reverse
--
-- reverse :: [a] -> [a]
-- 1) reverse is of type "a", accepts a list of type "a"s, and returns a list of type "a"s.
-- 2) reverse returns the reverse of the first argument list.
-- Examples) reverse [Red, Blue, Green] -> [Green, Blue, Red], reverse [1..5] -> [5,4,3,2,1]
-- 3) this function does apply to "color" types

-- snd
--
-- snd :: (a, b) -> b)
-- 1) snd is of type "a" and "b", accept a tuple, and returns an element of type "b"
-- 2) snd returns the second element of a tuple.
-- 3) snd (Yellow, Green) -> Green, snd (1, 100) -> 100
-- 3) this function does apply to "color" types

-- span
--
-- span :: (a -> Bool) -> [a] -> ([a], [a])
-- 1) span is of type "a", accepts two arguments, and returns a tupled list of type "a"
-- 2) span returns a tupled list, broken from the second argument's list.
-- Examples) span (/=5) [1, 5, 1] -> ([1], [5,1]), span (==Red) [Red, Blue, Green] -> ([Red], [Blue, Green])
-- 3) this function does apply to "color" types

-- splitAt
--
-- splitAt :: Int -> [a] -> ([a], [a])
-- 1) splitAt is of type "a", accepts two arguments, and returns a tupled list of type "a"
-- 2) splitAt returns a tupled list. The first part of the list is as long as the first argument, and the second part of the list is the cut out part.
-- Examples) splitAt 3 [1,2,5,6] -> ([1,2,5], [6]), splitAt 2 [Red, Blue, Green] -> ([Red, Blue], [Green])
-- 3) this function does apply to "color" types

-- zip
--
-- zip :: [a] -> [b] -> [(a, b)]
-- 1) zip is of type "a" and "b", accepts two arguments, and returns a listed tuple of type "a" and "b".
-- 2) zip returns a listed tuple, with each tuple match with one element from the first list with one element from the second list.
-- Examples) zip ['e', 'p'] ['z', 'z'] -> [('e', 'z'), ('p', 'z')], zip [Red, Blue, Green] [Red, Blue, Green] -> [(Red, Red), (Blue, Blue), (Green, Green)]
-- 3) this function does apply to "color" types
