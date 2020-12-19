import Data.Monoid
import Data.Foldable
-- Monoid is a typeclass that describes (via what it forces instances to implement)
--    1. _binary_ (i.e. between two arguments) operations that
--    2. must be associative (you can change the order but the result will remain the same)
--    3. and an identity operator must exist, which does not affect the result
--         for example - (this is like 0 in addition, 1 in multiplication and [] in concating lists)

-- OK so onto concrete examples, let's take the List type, which is a Monoid:
-- 1. the binary operation (which combines two of the same type) is called `mappend`. For lists, this is  equivalent to (++)
--       mappend [1,2] [3,4] ~~is the same as~~ [1,2] ++ [3,4]

-- 2. not so sure how associativity works? as in, if I flip the arguments of mappend above then the result is different
-- but perhaps associativity is about being able to do something like this
--             ([1] `mappend` [2]) `mappend` [3] <<<<<<<<< first evaluate the left 'pair'
--                   ~~is the  same as~~
--             [1] `mappend` ([2] `mappend` [3]) <<<<<<<<< first evaluate the right 'pair'

-- 3. the identity operator for list is the empty list:
--       mappend [1,2] [] ~~~~~~ [1,2]
--    the empty value must be exposed by a Monoid instance with `mempty`

-- mconcat :: [m] -> m is defined as
-- mconcat = foldr mappend mempty

-- ? WHY does it matter, why is it useful to have an instance of Monoid?
-- ? Or why is it helpful to think about these things?
-- ? Are there practical implications?
-- -------> from the book:
------------------ Typeclasses give us a way to recognize, organize, and use common functionalities
------------------ and patterns across types that differ in some ways but also have things in common.
-- -------> also from the book:
------------------ If you are dealing with monoidal data/operations, then you can safely parallelise the work
-- -------> this video (https://www.youtube.com/watch?v=BovTQeDK7XI&ab_channel=Tsoding) explains it via this example:

-- say we need to filter a character based on a number of predicates, one way to go about this is to just mix all predicates with a boolean operator

isAllowed :: Char -> Bool
isAllowed char =
  isA char || isB char

isA :: Char -> Bool
isA char = char == 'a'

isB :: Char -> Bool
isB char = char == 'b'

-- maybe we could refactor this to map all the predicates to the input and then use `any`

isAllowed' :: Char -> Bool
isAllowed' char =
  any ((==) True) . fmap (\predicate -> predicate char) $ [isA, isB]

-- or folding

isAllowed'' :: Char -> Bool
isAllowed'' char =
  foldr (\predicate acc -> acc || predicate char) True [isA, isB]

-- the insight from the video is that functions are Monoids, as long as what they return is a Monoid
-- our functions return a Bool, which is not a Monoid, because there are more than one monoidal boolean operations.
-- but Haskell includes a bunch of newtypes for these, for example Data.Monoid.Any / Data.Monoid.All, which are monoids.

-- finally, with the above in mind and knowing that Data.Foldable has a function `fold` that given a list of monoids
-- performs the operation off all of them, we  get:

isAllowedUsingMonoid :: Char -> Bool
isAllowedUsingMonoid char =
  getAny . fold predicates $ char
  where
    predicates = map (Any .) [isA, isB]

-- I wonder if `fold` given a list is basically mconcat?

isAllowedUsingMonoid' :: Char -> Bool
isAllowedUsingMonoid' char =
  getAny . mconcat predicates $ char
  where
    predicates = map (Any .) [isA, isB]

-------
------- So the next level is defining Monoids as a constraint for a 'bigger' type, like the one in the optional_monoid
------- exercise, where we define how to mappend (Optional a)s, so long as  `a` is a Monoid.

------- Another one of these is (->) - the function type, which is an instance of Monoid so long as the return type is.
------- I _think_ that one way to look at this is that mappend over functions is a subset of functional composition, but
------- where the type stays the same throughout the chain.

