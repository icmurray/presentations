module ValidationEither (
  mkPerson,
  validateUsername,
  validateAge,
  validateEmailAddress
) where

import Control.Applicative
import qualified Data.Text as T
import Text.Regex.Posix

import Person

-- Construct a valid Person
-- This time, the return type can provide additional information
-- if any argument is invalid.
mkPerson :: String -> Int -> String -> Either String Person
mkPerson username age email = Person <$> validUser <*> validAge <*> validEmail
  where validUser  = validateUsername username
        validAge   = validateAge age
        validEmail = validateEmailAddress email

--
--Username validation functions
--

validateUsername :: String -> Either String String
validateUsername s = notBlank s <* onlyContains usernameChars s

-- Validates that the given string is not blank after being
-- stripped of leading and trailing whitespace.
-- Returns the stripped string.
notBlank :: String -> Either String String
notBlank s | stripped  == "" = Left "Username is blank"
           | otherwise       = Right stripped
             where stripped = strip s

-- Validates that the given string only contains characters
-- belonging to the given list of allowed characters.
onlyContains :: [Char] -> String -> Either String String
onlyContains allowedChars s | allValid  = Right s
                            | otherwise = Left "Not valid chars!"
                              where allValid = all (flip elem allowedChars) s

--
--Age validation functions
--

validateAge :: Int -> Either String Int
validateAge age = nonNegative age <* notTooOld age

nonNegative :: Int -> Either String Int
nonNegative age | age < 0   = Left "Negative Age!"
                | otherwise = Right age

notTooOld :: Int -> Either String Int
notTooOld age | age > 150 = Left "Too old!"
              | otherwise = Right age

--
--Email validation functions
--

-- Overly simplified, and quite ugly.
-- A valid email address consists "local-part@domain" where "local-part" and
-- "domain"  may contain (a-z), (A-Z), (0-9) and periods.  
validateEmailAddress :: String -> Either String String
validateEmailAddress email | matches   = Right email
                           | otherwise = Left "Bad email address"
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


