module FixerUpper where

one =
  const <$> Just "Hello" <*> pure "World"

two =
  (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
