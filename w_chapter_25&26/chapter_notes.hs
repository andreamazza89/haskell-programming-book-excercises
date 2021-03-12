-- composing two Functors results in a Functor. The same is true for Applicative, but not Monad.

-- type composition means nesting one type inside the other; for example, if we had a type

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

-- then if we used it to compose two functors, the result is still a functor

instance (Functor f, Functor g) =>
  Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-- an example for this would be a list of Maybes. The Compose(d) version of which lets us `fmap` over the two structures
-- without having to nest the fmaps

-- There is a class for Functor composition in the standard library: Data.Functor.Compose


-- I think I need to develop an intuition for what happens when composing functions that take more than one parameter.
-- For example say we have `+` (a -> a -> a) and `-` (a -> a -> a), what's the result of composing them and is it useful?