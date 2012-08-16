module FunctorExamples where

import Data.List.Split (splitOn)

-- Maybe as a computation that may fail
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

-- List as non-deterministic choice
diceRoll :: Int -> [Int]
diceRoll n = [1..n]

-- An example of IO
readEmail :: IO String
readEmail = putStrLn "What's your email address? " >> getLine
    
-- A value that may contain a String
email :: Maybe String
email = Just "a.person@example.com"

-- A function for extracting the domain from an email address
extractDomain :: String -> String
extractDomain = last . splitOn "@"

-- A cumbersome function for extracting a domain from a Maybe String
extractDomainFromMaybe :: Maybe String -> Maybe String
extractDomainFromMaybe Nothing  = Nothing
extractDomainFromMaybe (Just e) = Just (extractDomain e)

-- Using functors to extract domains from strings
extractDomainFromFunctor :: Functor f => f String -> f String
extractDomainFromFunctor = fmap extractDomain

--
-- Implementing Functor examples
--

-- Maybe is already an instance of Functor, and it can't be
-- re-defined.  So for this example, let's define our own
-- maybe type.
data MyMaybe a = MyNothing
               | MyJust a
    deriving (Show, Eq)

instance Functor MyMaybe where
  fmap f (MyJust a) = MyJust (f a)
  fmap f MyNothing  = MyNothing

-- Test our MyMaybe.
myMaybeTest1 = fmap extractDomain (MyJust "a.person@example.com")
myMaybeTest2 = fmap extractDomain MyNothing

-- Let's try to define Either as an instance of Functor

-- It's actually already done for us in Control.Monad.Instances,
-- but it's not imported by default.  So we're free to define it
-- how we choose (unlike MyMaybe above), but in practice, you should
-- just import Control.Monad.Instances
-- import Control.Monad.Instances
--
-- Remember: 
instance Functor (Either a) where
  fmap g (Left a)  = Left a
  fmap g (Right b) = Right (g b)

buildEmail :: String -> String -> String
buildEmail name domain = name ++ "@" ++ domain


