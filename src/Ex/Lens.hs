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
  putStrLn "The next two should break law get-set by =/=:"
  putStrLn $ show $ set addCrewLens (view addCrewLens purplePearl) purplePearl
  putStrLn $ show $ purplePearl
  putStrLn "The next two should break law set-set by =/=:"
  putStrLn $ show $ set addCrewLens 100 (set addCrewLens 1 purplePearl)
  putStrLn $ show $ set addCrewLens 100 purplePearl


conditional :: Lens' (Bool, a, a) a
conditional = lens
  (\s -> if view _1 s then view _2 s else view _3 s)
  (\s a -> if view _1 s then set _2 a s else set _3 a s)
