module ApplicativeExamples where

import Control.Applicative

prompt :: String -> IO String
prompt msg = putStr (msg ++ "> ") >> getLine

buildEmail :: String -> String -> String
buildEmail name domain = name ++ "@" ++ domain

type Email = String

data Person = Person {
  name :: String,
  age :: Int,
  email :: Email
} deriving (Show, Eq)

promptName :: IO String
promptName = prompt "Name"

promptAge :: IO Int
promptAge = fmap read (prompt "Age")

promptEmail :: IO Email
promptEmail = prompt "Email"

--
-- Implementing Applicative examples
--

-- Maybe is already an instance of Functor, and it can't be
-- re-defined.  So for this example, let's define our own
-- maybe type.
data MyMaybe a = MyNothing
               | MyJust a
    deriving (Show, Eq)

-- We need to define MyMaybe as a Functor as-per the Appicative
-- type class definition.
instance Functor MyMaybe where
  fmap f (MyJust a) = MyJust (f a)
  fmap f MyNothing  = MyNothing

instance Applicative MyMaybe where
  pure x = MyJust x
  MyJust g  <*> MyJust x  = MyJust (g x)
  MyJust g  <*> MyNothing = MyNothing
  MyNothing <*> MyJust x  = MyNothing
  MyNothing <*> MyNothing = MyNothing

-- Test our MyMaybe.
myMaybeTest1 = buildEmail <$> MyJust "a.person" <*> MyJust "example.com"
myMaybeTest2 = buildEmail <$> MyJust "a.person" <*> MyNothing
myMaybeTest3 = buildEmail <$> MyNothing <*> MyJust "example.com"

-- Let's try to define Either as an instance of Applicative

-- It's already defined for us in Control.Monad.Instances, and it
-- can't be re-defined.  So for this example, we'll define our
-- own Either type.

data MyEither a b = MyLeft a
                  | MyRight b
  deriving (Show, Eq)

-- Again, we need to define Either as an instance of Functor.
instance Functor (MyEither a) where
  fmap g (MyLeft a)  = MyLeft a
  fmap g (MyRight b) = MyRight (g b)

instance Applicative (MyEither a) where
  pure x = MyRight x

  MyRight g <*> x = g `fmap` x
  MyLeft  err <*> x = MyLeft err

--   MyRight g <*> MyRight x = MyRight (g x)
--   MyRight g <*> MyLeft x  = undefined
--   MyLeft  g <*> MyRight x = undefined
--   MyLeft  g <*> MyLeft x  = undefined

-- Test our MyEither
myEitherTest1 = buildEmail <$> MyRight "a.person" <*> MyRight "example.com"
myEitherTest2 = buildEmail <$> MyRight "a.person" <*> MyLeft "Bad domain"
myEitherTest3 = buildEmail <$> MyLeft "Bad local" <*> MyLeft "Bad domain"




