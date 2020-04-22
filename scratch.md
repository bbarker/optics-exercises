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

## Polymorphic Optics

1. The type sig of the polymorphic lens which would allow changing a
`Vorpal x` to a `Vorpal y`: `Lens (Vorpal x) (Vorpal y) x y`

2. ```haskell
   data Preferences a b = Preferences { _best :: a , _worst :: b }
   ```

3. ```haskell
   data Result e = Result { _lineNumber :: Int, _result :: Either e String }

   result :: Lens (Result a) (Result b) (Either a String) (Either b String)
   ```

In this case we need to supply an entire `Either a _` since we may or may not have a wrapped `a`.

4. Yes

```haskell
silly :: Lens (a, b) (c, d) (a, b) (c, d)
silly = lens id (\_ a -> a)
```

5. `data Predicate a = Predicate (a -> Bool)`

```haskell
predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate :: lens (unPredicate) (Predicate)
```

### Lens Composition

1. `_2 . _1 . _2`

2. `mysteryDomino :: Lens' Eight Two`

3. ```haskell
   Lens Platypus BabySloth Armadillo Hedgehog
   Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)
   ```

4.  ```haskell
    spuzorktrowmble :: Lens Chumble Spuzz Gazork Trowlg
    gazorlglesnatchka :: Lens Gazork Trowlg Bandersnatch Yakka
    zinkattumblezz :: Lens Zink Wattoom Chumble Spuzz
    gruggazinkoom :: Lens Grug Pubbawup Zink Wattoom
    banderyakoobog :: Lens Bandersnatch Yakka Foob Mog
    boowockugwup :: Lens Boojum Jabberwock Grug Pubbawup
    snajubjumwock :: Lens Snark JubJub Boojum Jabberwock
    ````
    The trick seems to be looking at *either* the input
    type and focus for constructing the chain rather than
    worrying about it all at once.

    So:
      ```
        spuzorktrowmble : Chumble Gazork
        gazorlglesnatchka : Gazork Bandersnatch

        zinkattumblezz : Zink Chumble
        gruggazinkoom : Grug Zink

        banderyakoobog : Bandersnatch Foob

        boowockugwup : Boojum Grug
        snajubjumwock : Snark Boojum
      ```

      Gives:

      ```
      spuzorktrowmble . gazorlglesnatchka: Chumble Bandersnatch
      gruggazinkoom . zinkattumblezz : Grug Chumble

      ```

      And then:

      ```
        gruggazinkoom . zinkattumblezz
      . spuzorktrowmble . gazorlglesnatchka : Grug Bandersnatch

      ```

     And then:

      ```
        gruggazinkoom . zinkattumblezz
      . spuzorktrowmble . gazorlglesnatchka
      . banderyakoobog : Grug Foob

        snajubjumwock . boowockugwup: Snark Grug

      ```

      And then:

      ```
        snajubjumwock . boowockugwup
      . gruggazinkoom . zinkattumblezz
      . spuzorktrowmble . gazorlglesnatchka
      . banderyakoobog : Snark Foob

      ```

      So this tells us our actual lens function would have types from
      `snajubjumwock` and `banderyakoobog`:

      ```haskell
      Lens Snark JubJub Foob Mog
      ```
