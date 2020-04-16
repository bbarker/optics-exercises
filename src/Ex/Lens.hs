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
import Control.Lens.Unsound (lensProduct)
import Control.Applicative
import Data.Char
import Data.Foldable (fold)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Ex.Laws.Lens

data Ship = Ship {
    _name :: String
  , _numCrew :: Int
} deriving (Eq, Show)

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

---

type UserName = String
type UserId = String
data Session = Session {
  _userId :: UserId
, _userName :: UserName
, _createdTime :: String
, _expiryTime :: String
} deriving (Show, Eq)
makeLenses ''Session

-- Note: the input lenses must be disjoint for the
-- lens product to be lawful
userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName
-- Not disjoint:
alongsideUserId :: Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

---

data Err =
    ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
  deriving (Show, Eq)


conditional :: Lens' (Bool, a, a) a
conditional = lens
  (\s -> if view _1 s then view _2 s else view _3 s)
  (\s a -> if view _1 s then set _2 a s else set _3 a s)

-- fail get-set but pass set-get and set-set
msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode n) newMessage = ExitCode n

---
-- break 2nd and third law:
addCrewLens :: Lens' Ship Int
addCrewLens = lens _numCrew (\ship nc -> ship{_numCrew = nc +  _numCrew ship})

data Builder = Builder {
  _context :: [String]
, _build :: [String] -> String
}
-- These are not good instances
instance Eq Builder where
  a == b = (_context a) == (_context b)
instance Show Builder where
  show b = show (_context b)

-- This fails get-set: ["one ","two"] =/= ["one two"]
-- (due to not being able to reconstruct the original structure)
badBuilder :: Lens' Builder String
badBuilder = lens (\b -> (_build b) $ (_context b)) (\b s -> b{_context = [s]})

lensTests :: IO ()
lensTests = do
  putStrLn $ show $ view numCrew purplePearl
  let newPearl = set numCrew 41 purplePearl
  putStrLn $ show newPearl
  putStrLn $ show $ over numCrew (+3) purplePearl
  let session = Session "1234" "Joe Blo" "2019-07-25" "2019-08-25"
  putStrLn $ show $ view userInfo session
  let newSession = session{_userId="5678"}
  putStrLn $ show $ view alongsideUserId $
    (set alongsideUserId (newSession, "9999") session)
  putStrLn $ "Should break law get-set:"
  putStrLn $ getSetShow' addCrewLens purplePearl
  putStrLn $ "Should break law set-set:"
  putStrLn $ setSetShow' addCrewLens purplePearl 1 100
  let builderTwoStr = Builder {_context = ["one ", "two"], _build = fold}
  putStrLn $ "_build . _context == " <>
    ((_build builderTwoStr) $ (_context builderTwoStr))
  putStrLn $ setGetShow' badBuilder builderTwoStr "three"
  putStrLn $ getSetShow' badBuilder builderTwoStr
  putStrLn $ setSetShow' badBuilder builderTwoStr "three" "four"
