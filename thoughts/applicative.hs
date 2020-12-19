-- Applicative is initially defined as compared to Functor, as it involves a very similar function to fmap:
---- fmap ::   (a -> b) -> f a -> f b
---- ap   :: f (a -> b) -> f a -> f b

-- so the only difference there is that  the mapping  function is  wrapped  into the same structure as  the argument,
-- for example: Just (+ 1) <*> Just 41  ---> Just 42

-- not sure about its significance yet, but an Applicative also must have 'implement' *pure*, which takes a value and
-- 'wraps' it in the structure, for example: (pure 1) :: Maybe Int ----> Just 1

-- one interesting thing is that for some structures, the structure must be a Monoid in order for the whole thing to be
-- Applicative; Tuple is an example: ("Woo", (+1)) <*> (" Hoo!", 0) ----> ("Woo Hoo!", 1)

-- Laws:
---- id (pure id <*> f == f)
---- composition (pure (.) <*> [(+1)] <*> [(*2)] <*> [1,2,3] == [(+1)] <*> ([(*2)] <*> [1, 2, 3]))
---- homomorphism (pure f <*> pure x = pure (f x))
---- interchange (pure (+ 1) <*> pure 2   =====     pure ($ 2) <*> pure (+ 1))