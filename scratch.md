## Lenses

```haskell
over :: Lens' (Bool, Int) Int -> (Int -> Int) -> (Bool -> Int) -> (Bool -> Int)

wand :: Lens' Inventory [Potion]

gazork :: Lens' Chumble Spuzz


```

Remember that `makeLenses` needs to be used before any usage
of the lenses.

Focus on 2nd element of three-tuple: `_2 :: Lens' (a,b,c) b`

`inMaybe :: Lens' (Maybe a) a` isn't possible, as it isn't always
possible to get an `a` from a `Maybe a`.
Same for `Lens' (Either a b) a`. Similarly,
`listThird :: Lens' [a] a` is not valid as a list may not have 3
elements (or any!).


```haskell
conditional :: Lens' (Bool, a, a) a
conditional = lens
  (\s -> if view _1 s then view _2 s else view _3 s)
  (\s a -> if view _1 s then set _2 a s else set _3 a s)
```

`msg :: Lens' Err String` doesn't exist because `Err` may or may not
have a constructor with a `String` parameter, for:

```haskell
data Err =
ReallyBadError { _msg :: String }
| ExitCode { _code :: Int }
```

Answer, actually you can: What happens when you’re passed an
`ExitCode` instead of `ReallyBadError`? Well, the setter could
just do nothing, but what about the getter? Since you know it needs
a string I suppose you could
just return an empty string right? This would compile but it still
just seems wrong doesn’t it? In fact this would violate the lens laws.