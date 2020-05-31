{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}

module Ex.Folds where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Ex.Util as U

data Card = Card { _name    :: String
  , _aura    :: Aura
  , _holo    :: Bool -- Is the card holographic
  , _moves   :: [Move]
  } deriving (Show, Eq)

data Move = Move {
    _moveName  :: String
  , _movePower :: Int
  } deriving (Show, Eq)

-- Each card has an aura-type
data Aura
  = Wet
  | Hot
  | Spark
  | Leafy
  deriving (Show, Eq)

makeLenses ''Card
makeLenses ''Move

deck = [
    Card "Skwortul"    Wet False   [Move "Squirt" 20]
  , Card "Scorchander" Hot False   [Move "Scorch" 20]
  , Card "Seedasaur"   Leafy False [Move "Allergize" 20]
  , Card "Kapichu"     Spark False [Move "Poke" 10 , Move "Zap" 30]
  , Card "Elecdude"    Spark False [Move "Asplode" 50]
  , Card "Garydose"    Wet True    [Move "Gary's move" 40]
  , Card "Moisteon"    Wet False   [Move "Soggy" 3]
  , Card "Grasseon"    Leafy False [Move "Leaf Cut" 30]
  , Card "Spicyeon"    Hot False   [Move "Capsaicisize" 40]
  , Card "Sparkeon"    Spark True  [Move "Shock" 40 , Move "Battery" 50]
  ]

foldTests :: IO ()
foldTests = do
  putStrLn "\n\n  -- Fold Tests --\n"
