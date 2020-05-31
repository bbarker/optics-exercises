module Main where

import Ex.Folds (foldTests)
import Ex.Lens (lensTests)

main :: IO ()
main = do
  lensTests
  foldTests
