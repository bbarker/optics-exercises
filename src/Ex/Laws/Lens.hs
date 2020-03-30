{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}

module Ex.Laws.Lens where

import Control.Lens
import Control.Lens.Unsound (lensProduct)
import Control.Applicative
import Control.Exception (assert)
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Ex.Util as U

setGet' :: Lens' s a -> s -> a -> a
setGet' myLens s a = view myLens (set myLens a s)

setGetIsEq' :: Eq a => Lens' s a -> s -> a -> Bool
setGetIsEq' myLens s a = a == setGet' myLens s a

setGetShow' :: (Eq a, Show a, Show s) => Lens' s a -> s -> a -> String
setGetShow' myLens s a = "set-get for " <> show a <> " in " <> show s <> ": "
  <> U.showEq a (setGet' myLens s a)

getSet' :: Lens' s a -> s -> s
getSet' myLens s =  set myLens (view myLens s) s

getSetIsEq' :: Eq s => Lens' s a -> s -> Bool
getSetIsEq' myLens s = s == getSet' myLens s

getSetShow' :: (Eq s, Show s) => Lens' s a -> s -> String
getSetShow' myLens s = "get-set for " <> show s <> ": "
  <> U.showEq s (getSet' myLens s)

setSet' :: Eq a => Lens' s a -> s -> a -> a -> (s, s)
setSet' myLens s a a' = assert (not $ a == a') (
    set myLens a' s
  , set myLens a' (set myLens a s)
  )

setSetIsEq' :: (Eq a, Eq s) => Lens' s a -> s -> a -> a -> Bool
setSetIsEq' myLens s a a' = (fst ss') == (snd ss')
  where ss' = setSet' myLens s a a'

setSetShow' :: (Eq a, Eq s, Show a, Show s) => Lens' s a -> s -> a -> a -> String
setSetShow' myLens s a a' = "set-set for " <> show a <> ", " <> show a'
  <> " in " <> show s <> ": "
  <> U.showEq (fst ss') (snd ss')
  where ss' = setSet' myLens s a a'
