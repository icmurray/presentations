module Person where

import qualified Data.Map as M

type Email = String  -- just a synonym

data Person = Person {
  username :: String,
  age :: Int,
  email :: Email
} deriving (Show, Eq)

