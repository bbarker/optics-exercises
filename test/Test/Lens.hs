{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}

module Test.Lens where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Ship = Ship {
    _name :: String
  , _numCrew :: Int
} deriving (Show)

numCew :: Lens' Ship Int
numCew = lens _numCrew (\ship nc -> ship{_numCrew = nc})
name :: Lens' Ship String
name = lens _name (\ship n -> ship{_name = n})
