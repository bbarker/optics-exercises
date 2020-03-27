{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}

module Ex.Lens where

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

{-
numCrew :: Lens' Ship Int
numCrew = lens _numCrew (\ship nc -> ship{_numCrew = nc})
name :: Lens' Ship String
name = lens _name (\ship n -> ship{_name = n})
-}
makeLenses ''Ship

purplePearl :: Ship
purplePearl = Ship {
  _name = "Purple Pearl"
, _numCrew = 38
}

conditional :: Lens' (Bool, a, a) a
conditional = lens
  (\s -> if view _1 s then view _2 s else view _3 s)
  (\s a -> if view _1 s then set _2 a s else set _3 a s)

lensTests :: IO ()
lensTests = do
  putStrLn $ show $ view numCrew purplePearl
  let newPearl = set numCrew 41 purplePearl
  putStrLn $ show newPearl
  putStrLn $ show $ over numCrew (+3) purplePearl
