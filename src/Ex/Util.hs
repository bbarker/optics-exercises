module Ex.Util where

neq :: String
neq = " =/= "

eq :: String
eq = " === "

showEq :: (Eq a, Show a) => a -> a -> String
showEq a1 a2 =
  if a1 == a2 then "refl: " <> a1s else a1s <> neq <> a2s
  where
    a1s = show a1
    a2s = show a2