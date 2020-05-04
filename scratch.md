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

## Operators

1.
    * ```haskell
       -- Untested:
       duloc & name %~ (<> " a perfect place")
         & gate . open ~. False
         & army . knights %~ (+28)
       -- Book answer:
       duloc
         & gate . open &&~ False
         & army . knights *~ 3
         & name <>~ ": a perfect place"
       ```
    * ```haskell
       -- Untested:
       duloc & name <>~ "instein"
         & army . archers -~ 5
         & army . knights +~ 12
         & gate . oilTemp *~ 10
       -- Book answer:
       duloc
         & gate . oilTemp ^~ 2
         & army . archers -~ 5
         & army . knights +~ 12
         & name <>~ "instein"
       ```
    *  Note: Get old focus in addition to setting new one (addition: `<<+~ 15`)
       ```haskell
       -- Untested:
       duloc & gate . oilTemp /~ 2
         & name <<<>~ ": Home"
         & ._2 name <>~ " of the talking Donkeys"
       -- Book answer (looks like I found the simplified solution but was off on symbol names):
       duloc
         & name <<>~ ": Home"
         & _2 . name <>~ " of the talking Donkeys"
         & _2 . gate . oilTemp //~ 2
       ```

2.
    * `(False, "opossums") `undefined` _1 ||~ True` => `(True, "opossums")`; so `undefined = &`
    * 2 & id `undefined` 3 => `6`, so `undefined = *~`
    * Starting at `((True, "Dudley"), 55.0)`
      ```haskell
      >>> import Data.Char (toUpper)
      >>> ((True, "Dudley"), 55.0)
      & _1 . _2 `undefined` " - the worst"
      & _2 `undefined` 15
      & _2 `undefined` 2
      & _1 . _2 `undefined` map toUpper
      & _1 . _1 `undefined` False
      ((False,"DUDLEY - THE WORST"),20.0)
      ```

      My answer:
      ```haskell
      >>> import Data.Char (toUpper)
      >>> ((True, "Dudley"), 55.0)
      & _1 . _2 <>~ " - the worst"
      & _2 -~ 15
      & _2 //~ 2
      & _1 . _2 %~ map toUpper
      & _1 . _1 .~ False -- book used &&~
      ((False,"DUDLEY - THE WORST"),20.0)
      ```
3. `view` (or `^.`)
4. `%~ :: s -> Lens s t a b -> (a -> b) -> t`

   Ooops, it is actually in the wrong order, it should be:
  `%~ :: Lens s t a b -> (a -> b) -> s -> t`.
   This should be easier to notice if we remember `&` has fixity 0
   (lowest priority).


## Folds

### Introduction to Folds

1.
    ```haskell
    beastSizes :: [(Int, String)]
    beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
    >>> beastSizes ^.. folded
    -- ... id? i.e. returns the same list
    -- Answer: Correct
    >>> beastSizes ^.. folded . folded
    -- ... ?  ["Sirens", "Kraken", "Ogopogo"]
    -- Answer: Correct
    >>> beastSizes ^.. folded . folded . folded
    -- ... ? "SirensKrakenOgopogo"
    -- Answer: Correct
    >>> beastSizes ^.. folded . _2
    -- ... ?  ["Sirens", "Kraken", "Ogopogo"]
    -- Answer: Correct
    >>> toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]]
    -- [1, 2, 3, 4, 5, 6]
    -- Answer: Correct
    >>> toListOf
      (folded . folded)
      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
    -- "Captain First Mate"
    -- Answer  ... almost : "CaptainFirst Mate"
    -- Operator syntax:
    (M.fromList [("Jack", "Captain"), ("Will", "First Mate")]) ^.. folded . folded

    >>> ("Hello", "It's me") ^.. both . folded
    -- ? "HelloIt's me"
    -- Answer: Correct
    >>> ("Why", "So", "Serious?") ^.. each
    -- ["Why", "So", "Serious?"]
    -- Answer: Correct
    :set +m
    let quotes :: [(T.Text, T.Text, T.Text)];
        quotes = [("Why", "So", "Serious?"), ("This", "is", "SPARTA")]
    >>> quotes ^.. each . each . each
    -- ? "WhySoSerious?ThisisSPARTA"
    -- Answer: Correct
    ```
2.
    ```haskell
    folded and _1
    >>> toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
    [1, 2, 3]
    -- folded :: Fold [(Int, Char)] (Int, Char)
    -- _1 :: Fold (Int, Char) Int
    -- Answer: Correct

    folded, _2, and toListOf
    >>> toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
    ["one", "two", "three"]
    -- folded :: Fold (Set String) String
    -- _2 :: Fold (Bool, Set String) (Set String)
    -- toListOf :: Fold (Bool, Set String) String -> (Bool, Set String) -> [String]
    -- Answer: correct, though no answer for toListOf shown

    folded and folded and toListOf
    >>> toListOf
    (folded . folded)
    (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
    "CaptainFirst Mate"

    -- folded :: Fold (Map String String) String
    -- folded :: Fold String Char
    -- toListOf :: Fold (Map String String) [Char] -> Map String String -> [Char]
    -- Oops, first [Char] should be [Char]:
    -- toListOf :: Fold (Map String String) Char -> Map String String -> [Char]
    -- Note: String == [Char]
    ```

3.
    ```haskell
    >>> [1, 2, 3] ^.. _
    [1, 2, 3]
    -- folded
    >>> ("Light", "Dark") ^.. _
    ["Light"]
    -- _1
    >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. _
    ["Light","Dark","Happy","Sad"]
    -- folded . each
    -- Book answer: `folded . both`, both answers work though
    >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. _
    ["Light","Happy"]
    -- folded . _1
    >>> [("Light", "Dark"), ("Happy", "Sad")] ^.. _
    "DarkSad"
    -- folded . _2 . folded
    >>> ("Bond", "James", "Bond") ^.. _
    ["Bond","James","Bond"]
    -- each
    ```

### Custom Folds

1. Fill in each blank with either `to`, `folded`, or `folding`.
    ```haskell
    >>> ["Yer", "a", "wizard", "Harry"] ^.. folded . _
    "YerawizardHarry"
    -- folded
    >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
    [1, 2, 4, 5]
    -- ?
    -- Answer: folding: folding (take 2) transforms each list into
    --   a shorter list, which at first seems like what `to` is
    --   doing below. But remember, `folding` is treating what it
    --   returns as a Fold, so they will be "unwrapped" and folded over.
    >>> [[1, 2, 3], [4, 5, 6]] ^.. folded . _ (take 2)
    [[1,2], [4,5]]
    -- to
    >>> ["bob", "otto", "hannah"] ^.. folded . _ reverse
    ["bob", "otto", "hannah"]
    -- to
    >>> ("abc", "def") ^.. _ (\(a, b) -> [a, b]). _ reverse . _
    "cbafed"
    -- folding, to, folded:
    -- ("abc", "def") ^.. folding  (\(a, b) -> [a, b]) . to reverse . folded
    -- This seems a bit over the top as you could also do:
    -- ("abc", "def") ^.. each . to reverse . folded
    ```
2. Fill in the blank for each of the following expressions with a path of folds which results in the specified answer. Avoid partial functions and `fmap`.
    ```haskell
    >>> [1..5] ^.. _
    [100,200,300,400,500]
    -- [1..5] ^.. folded . to (*100)
    >>> (1, 2) ^.. _
    [1, 2]
    -- (1, 2) ^.. both
    -- Alternative from book: (1, 2) ^.. folding (\(a, b) -> [a, b])
    >>> [(1, "one"), (2, "two")] ^.. _
    ["one", "two"]
    -- [(1, "one"), (2, "two")] ^.. folded . to snd
    >>> (Just 1, Just 2, Just 3) ^.. _
    [1, 2, 3]
    -- (Just 1, Just 2, Just 3) ^.. each & catMaybes
    -- Book solution (more idiomatic optics though longer):
    -- (Just 1, Just 2, Just 3)
    --  ^.. folding (\(a, b, c) -> [a, b, c]) . folded
    >>> [Left 1, Right 2, Left 3] ^.. _
    [2]
    -- [Left 1, Right 2, Left 3] ^.. folded . folded
    >>> [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. _
    [1, 2, 3, 4, 5, 6, 7, 8]
    -- [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . each . folded
    -- Book: ^.. folded . folding (\(a, b) -> a <> b)
    >>> [1, 2, 3, 4] ^.. _
    [Left 1, Right 2, Left 3, Right 4]
    -- ^.. folded . to (\x -> if even x then Right x else Left x)
    >>> [(1, (2, 3)), (4, (5, 6))] ^.. _
    [1, 2, 3, 4, 5, 6]
    -- I doubt this is the desired solution but:
    -- ^.. folded . to (\x -> fst x : (biList $ snd x)) . folded
    -- As usual, book solution is to use `folding` in the hairy cases:
    -- ^.. folded . folding (\(a, (b, c)) -> [a, b, c])
    >>> [(Just 1, Left "one"), (Nothing, Right 2)] ^.. _
    [1, 2]
    -- While I could think of a way to get this, nothing easy or optical is coming to mind
    -- Of course, the answer involves `folding`, but, it also uses nested
    -- actions/folds, indicating it is indeed rather tricky to do with
    -- optics:
    -- ^.. folded . folding (\(a, b) -> a ^.. folded <> b ^.. folded)
    >>> [(1, "one"), (2, "two")] ^.. _
    [Left 1, Right "one", Left 2, Right "two"]
    -- [(1, "one"), (2, "two")] ^.. folded . to (\x -> [Left $ fst x, Right $ snd x]) . folded
    -- Book uses folding with better pattern matching:
    -- ^.. folded . folding (\(a, b) -> [Left a, Right b])
    >>> S.fromList ["apricots", "apples"] ^.. _
    "selppastocirpa"
    -- Not sure what `S` is for the module
    -- ^.. folded . folded . reverse
    -- But `["apricots", "apples"] ^.. folded . folded ` gives a type error
    -- Aha, this  issue was likely a result of using `-XOverloadedStrings`
    -- However, I missed using `&`:
    -- ^.. folded . folded & reverse
    -- Book solution is to again use folding:
    -- ^.. folded . folding reverse "selppastocirpa"
    ```
3. *(Bonus)*

    ```haskell
    >>> [(12, 45, 66), (91, 123, 87)] ^.. _
    "54321"
    -- ^.. folded . folding (\(a,b,c) -> [b]) . to show . folding reverse
    -- Book reminds me to use `_2` even on n-tuples:
    --  ^.. folded . _2 . to show . folding reverse
    >>> [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. _
    ["b", "d"]
    -- ^.. folding (filter (even . fst)) . folded
    -- Cool, was shorter than the book answer:
    -- ^.. folded . folding (\(a, b) -> if (even a) then return b else [])
    -- Sometimes it still makes sense to do non-optics stuff earlier
    -- (like filter)
    ```
