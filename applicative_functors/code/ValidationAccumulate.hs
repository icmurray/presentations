module ValidationAccumulate (
  mkPerson,
  validateUsername,
  validateAge,
  validateEmailAddress
) where

import Control.Applicative
import qualified Data.Text as T
import Text.Regex.Posix

import Person

--
--Data types
--

data ValidationAcc e a = Failure [e] | Success a
  deriving (Show, Eq, Ord)

-- First of all, we need to define (ValidationAcc e) as an instance of
-- Functor
instance Functor (ValidationAcc e) where
  fmap g (Failure errs) = Failure errs
  fmap g (Success a)    = Success (g a)

-- Now we can define it as an instance of Applicative
instance Applicative (ValidationAcc e) where
  pure x = Success x

  Success g   <*> Success a   = Success (g a)
  Failure es  <*> Success a   = Failure es
  Success g   <*> Failure es  = Failure es
  Failure e1s <*> Failure e2s = Failure (e1s ++ e2s)


-- Construct a valid Person
-- This time, the return type can provide additional information
-- if any argument is invalid.
mkPerson :: String -> Int -> String -> ValidationAcc String Person
mkPerson username age email = Person <$> validUser <*> validAge <*> validEmail
  where validUser  = validateUsername username
        validAge   = validateAge age
        validEmail = validateEmailAddress email

--
--Username validation functions
--

validateUsername :: String -> ValidationAcc String String
validateUsername s = notBlank s <* onlyContains usernameChars s

-- Validates that the given string is not blank after being
-- stripped of leading and trailing whitespace.
-- Returns the stripped string.
notBlank :: String -> ValidationAcc String String
notBlank s | stripped  == "" = Failure ["Blank username"]
           | otherwise       = Success s
             where stripped = strip s

-- Validates that the given string only contains characters
-- belonging to the given list of allowed characters.
onlyContains :: [Char] -> String -> ValidationAcc String String
onlyContains allowedChars s | allValid  = Success s
                            | otherwise = Failure ["Invalid characters in username"]
                              where allValid = all (flip elem allowedChars) s

--
--Age validation functions
--

validateAge :: Int -> ValidationAcc String Int
validateAge age = nonNegative age <* notTooOld age

nonNegative :: Int -> ValidationAcc String Int
nonNegative age | age < 0   = Failure ["Negative age!"]
                | otherwise = Success age

notTooOld :: Int -> ValidationAcc String Int
notTooOld age | age > 150 = Failure ["Too old!"]
              | otherwise = Success age

--
--Email validation functions
--

-- Overly simplified, and quite ugly.
-- A valid email address consists "local-part@domain" where "local-part" and
-- "domain"  may contain (a-z), (A-Z), (0-9) and periods.  
validateEmailAddress :: String -> ValidationAcc String String
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


