module ValidationRich (
  mkPerson,
  validateUsername,
  validateAge,
  validateEmailAddress
) where

import Control.Applicative
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Text.Regex.Posix

import Person

--
--Data types
--

data ValidationRich e a = Failure e | Success a
  deriving (Show, Eq, Ord)

-- First of all, we need to define (ValidationRich e) as an instance of
-- Functor
instance Functor (ValidationRich e) where
  fmap g (Failure err) = Failure err
  fmap g (Success a)   = Success (g a)

-- Now we can define it as an instance of Applicative
instance (Monoid e) => Applicative (ValidationRich e) where
  pure x = Success x

  Success g   <*> Success a   = Success (g a)
  Failure es  <*> Success a   = Failure es
  Success g   <*> Failure es  = Failure es
  Failure e1s <*> Failure e2s = undefined

-- Construct a valid Person
mkPerson :: String -> Int -> String -> ValidationRich [String] Person
mkPerson username age email = Person <$> validUser <*> validAge <*> validEmail
  where validUser  = validateUsername username
        validAge   = validateAge age
        validEmail = validateEmailAddress email

--
--Username validation functions
--

validateUsername :: String -> ValidationRich [String] String
validateUsername s = notBlank s <* onlyContains usernameChars s

-- Validates that the given string is not blank after being
-- stripped of leading and trailing whitespace.
-- Returns the stripped string.
notBlank :: String -> ValidationRich [String] String
notBlank s | stripped  == "" = Failure ["Blank username"]
           | otherwise       = Success s
             where stripped = strip s

-- Validates that the given string only contains characters
-- belonging to the given list of allowed characters.
onlyContains :: [Char] -> String -> ValidationRich [String] String
onlyContains allowedChars s | allValid  = Success s
                            | otherwise = Failure ["Invalid characters in username"]
                              where allValid = all (flip elem allowedChars) s

--
--Age validation functions
--

validateAge :: Int -> ValidationRich [String] Int
validateAge age = nonNegative age <* notTooOld age

nonNegative :: Int -> ValidationRich [String] Int
nonNegative age | age < 0   = Failure ["Negative age!"]
                | otherwise = Success age

notTooOld :: Int -> ValidationRich [String] Int
notTooOld age | age > 150 = Failure ["Too old!"]
              | otherwise = Success age

--
--Email validation functions
--

-- Overly simplified, and quite ugly.
-- A valid email address consists "local-part@domain" where "local-part" and
-- "domain"  may contain (a-z), (A-Z), (0-9) and periods.  
validateEmailAddress :: String -> ValidationRich [String] String
validateEmailAddress email | matches   = Success email
                           | otherwise = Failure ["Invalid email address"]
  where matches = email =~ ("^" ++ emailRegex ++ "$") :: Bool
        emailRegex = local ++ "@" ++ domain
        local = validChars
        domain = validChars
        validChars = "[.a-zA-Z0-9]+"

--
--Private helper functions
--

usernameChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "-_ "

-- strip leading and trailing whitespace
strip :: String -> String
strip = T.unpack . T.strip . T.pack


newtype AccMap k v = AccMap { toMap :: M.Map k v }
  deriving (Show, Eq)

instance (Ord k, Monoid v) => Monoid (AccMap k v) where
  mempty                        = AccMap M.empty
  AccMap m1 `mappend` AccMap m2 = AccMap (M.unionWith mappend m1 m2)

enrich :: String -> ValidationRich e a -> ValidationRich (M.Map String e) a
enrich _   (Success a)   = Success a
enrich key (Failure err) = Failure (M.singleton key err)

