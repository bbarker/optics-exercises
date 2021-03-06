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

### Fold Actions

1.
    ```haskell
    >>> _ folded []
    False
    -- anyOf folded [] -- wrong, look at the type signature!
    -- has folded []
    >>> _ both ("Yo", "Adrian!")
    "YoAdrian!"
    -- sumOf? Nope, need a Num a
    -- foldOf both ("Yo", "Adrian!")
    >>> _ each "phone" ("E.T.", "phone", "home")
    True
    -- elemOf each "phone" ("E.T.", "phone", "home")
    >>> _ folded [5, 7, 2, 3, 13, 17, 11]
    Just 2
    -- minimumOf folded [5, 7, 2, 3, 13, 17, 11]
    >>> _ folded [5, 7, 2, 3, 13, 17, 11]
    Just 11
    -- lastOf folded [5, 7, 2, 3, 13, 17, 11]
    >>> _ folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
    -- anyOf folded ((> 9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
    >>> _ folded even [11, 22, 3, 5, 6]
    Just 22
    -- findOf folded even [11, 22, 3, 5, 6]
    ```
2. ```haskell
   let input = ["umbrella", "olives", "racecar", "hammer"]
   findOf folded (== "racecar") input
   -- Just "racecar"
   -- Oh but this is cheating, we nee dto find using the criteria that it is a palindome.  But easy enough:
   findOf folded (\x -> x == reverse x) input

   let input = (2, 4, 6)
   allOf each even input
   -- True

   let input = (1, 2)
   sumOf each input
   -- 3
   -- book's solution more general but longer:
   -- getSum $ foldMapOf both Sum (1, 2)

3. (Bonus)

   ```haskell
   let input = "Do or do not, there is no try."
   let countVowels = (\s -> length $ filter (\c -> any (\v -> v == c) "aeiou") s)
   maximumByOf folded (comparing countVowels) (words input)
   -- Just "there"
   -- Book solution a bit shorter in total due to use of `elem`
   maximumByOf (folding words) (compare `on` (length . filter (`elem` "aeiouy"))) input
   -- A variation merging the two approaches:
   maximumByOf folded (comparing (length . filter (`elem` "aeiouy"))) (words input)

   let input = ["a", "b", "c"]
   foldMapOf folded id input & reverse -- Not just an optic but ...
   -- "cba"
   -- book answers:
   >>> foldByOf folded (flip (++)) "" ["a", "b", "c"]
   "cba"
   -- The following also works
   >>> import Data.Monoid (Dual(..))
   >>> getDual $ foldMapOf folded Dual ["a", "b", "c"]
   "cba"

   let input = [(12, 45, 66), (91, 123, 87)]
   -- no: foldMapOf folded . to (toListOf ((to snd) . each)) input
   -- this at least folds over everything:
   toListOf (folded . each) input
   -- closer!:
   toListOf (folded . _2) input
   -- [45,123]
   -- nope: toListOf (folded . reverse . show . _2) input
   -- nope: toListOf (folded . show . _2) input
   -- back on the right track:
   toListOf ((folded . _2) . (to show)) input
   -- the tricky part here was remembering to stick `folded` at the end, as we
   -- want to focus on each character *after* the other transformations
   toListOf ((folded . _2) . (to $ reverse . show) . folded) input
   -- The book cleaned this up slightly (more calls to `to` but clearer):
   toListOf (folded ._2 . to show . to reverse . folded) input

   let input = [(1, "a"), (2, "b"), (3, "c"), (4, "d")]
   -- nope: toListOf (folded . even `on` fst . _2) input
   -- I was reaching for filtered but haven't  learned it yet, book
   -- answer uses folding:
   input ^.. folded . folding (\(a, b) -> if (even a) then pure b else [])

   ```

### Higher Order Folds

1.
   ```haskell
    >>> "Here's looking at you, kid" ^.. _ 7 folded
    "looking at you, kid"
    -- "Here's looking at you, kid" ^.. dropping 7 folded

    >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 _
    ["My","Hakuna","No"]
    -- ["My Precious", "Hakuna Matata", "No problemo"] ^..
    --  folded . taking 1 (to words . folded)
    -- Note: use `to words . folded` instead of just `to words` because `to words` only
    -- focuses at the list level, but we need to focus into each generated list
    -- Book solution better using `worded`:
    -- ^.. folded . taking 1 worded

    >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. _
    ["My"]
    -- ["My Precious", "Hakuna Matata", "No problemo"] ^..
    --  taking 1 (folded . taking 1 (to words . folded))

    >>> ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . _
    "MyHakunaNo"
    -- ["My Precious", "Hakuna Matata", "No problemo"] ^.. folded . taking 1 (to words . folded) . folded

    >>> _ (10, 50, 100)
    60
    -- sumOf (takingWhile (<= 50) each) (10, 50, 100)
    -- Book solution:
    -- sumOf (taking 2 each) (10, 50, 100)

    >>> ("stressed", "guns", "evil") ^.. _ each
    ["evil","guns","stressed"]
    -- ("stressed", "guns", "evil") ^.. backwards each


    >>> ("stressed", "guns", "evil") ^.. backwards each . to _
    ["live","snug","desserts"]
    -- ("stressed", "guns", "evil") ^.. backwards each . to reverse

    >>> import Data.Char (isAlpha)
    >>> "blink182 k9 blazeit420" ^.. _
    >>> "1829420"
    --  "blink182 k9 blazeit420" ^.. to words . folded . to reverse . takingWhile (not . isAlpha) folded
    -- This gets all the numbers together, but need to reverse them first while
    -- each sublist is still preserved:
    -- "blink182 k9 blazeit420" ^.. to words . folded . to reverse . takingWhile (not . isAlpha) folded
    -- The trick is to use backwards in the right place:
    --"blink182 k9 blazeit420" ^.. to words . folded . to reverse . backwards (takingWhile (not . isAlpha) folded)
    -- Book solution way better (key is to use drop not take):
    -- ^.. worded . droppingWhile isAlpha folded
    ```
2.
   ```haskell
   sample :: [Int]
   sample = [-10, -5, 4, 3, 8, 6, -2, 3, -5, -7]
   sample ^.. takingWhile (< 0) folded & length
   -- ^ # measurements before thaw
   -- Book uses lengthOf fold action:
   lengthOf (takingWhile (<= 0) folded) sample

   maximumOf (taking 4 folded) sample
   -- ^ # warmest in first 4 days

   sample ^? dropping 1 (droppingWhile (/= 4) folded)
   -- ^ get temp on day after a given temp

   foldByOf folded (\t s -> if t < 0.0 then (s + 1) else 0) 0 sample
   -- ^ max # consecutive days below freezing (didn't use HOF)
   -- I feel the book solution, while it does use HOFs, is not
   -- as clear, and maybe not as correct in general:
   lengthOf (takingWhile (<0) (backwards folded)) sample

   sample ^.. takingWhile (> 0) (droppingWhile (< 0) folded)
   -- ^ collected the first stretch of positive temps

   let trimmingWhile :: (a -> Bool) -> Fold s a -> Fold s a;
       trimmingWhile pred fsa = takingWhile pred (droppingWhile (not . pred) fsa)
   sample ^.. trimmingWhile (> 0) folded

   ```


# Temporary Note: Skiping ahead to prisms

## Prisms

### Introduction to Prisms

 1. Which prisms will be generated from the following data declaration? Give their names and types.

  ```haskell
  data ContactInfo =
      Email String
    | Telephone Int
    | Address String String String
  makePrisms ''ContactInfo
  ```
  Answer:
  ```haskell
  _Email :: Prism' ContactInfo String
  _Telephone :: Prsim' ContactInfo Int
  _Address :: Prsim' ContactInfo (String, String, String)
  ```
