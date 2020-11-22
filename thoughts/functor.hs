-- Functor is about types of data that have some sort of structure that contains other data.
---- for example, (Maybe a) is a container for whatever a is and [a] is a a container for  whatever a is.

---- the only function a Functor has to 'implement' in order to be called a functor is `fmap`.
---- fmap is basically taking a function and applying in to the content of the container. for example, in
---- || fmap (+ 1) (Maybe 1) ||, the function is only applied to the  content of Maybe, giving || Maybe 2 || as a result.

-- For a Functor to be a functor, it must respect two laws, namely the identity and composability laws:

-- identity: fmap id _ == id _

-- composability: fmap (f . g) _ == (f . g) _
