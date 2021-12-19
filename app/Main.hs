{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fplugin=Plugin #-}

import Data.Foldable

f x = partial case x of
  True -> "hello"

g = partial \case
  True -> "hello"

main = do
  for_ [(a, b) | a <- [f, g], b <- [True, False]] \(f, x) -> do
    print (f x :: Maybe String)
