module ValidationMaybe (
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
-- Normally this would live in the Person module, but we're going to
-- look at different return types, in this case Maybe Person
mkPerson :: String -> Int -> String -> Maybe Person
mkPerson username age email = Person <$> validUser
                                     <*> validAge <*> validEmail
  where validUser  = validateUsername username
        validAge   = validateAge age
        validEmail = validateEmailAddress email

--
--Username validation functions
--

validateUsername :: String -> Maybe String
validateUsername s = notBlank s <* onlyContains usernameChars s

-- Validates that the given string is not blank after being
-- stripped of leading and trailing whitespace.
-- Returns the stripped string.
notBlank :: String -> Maybe String
notBlank s | stripped  == "" = Nothing
           | otherwise       = Just stripped
  where stripped = strip s

-- Validates that the given string only contains characters
-- belonging to the given list of allowed characters.
onlyContains :: [Char] -> String -> Maybe String
onlyContains allowedChars s | allValid  = Just s
                            | otherwise = Nothing
  where allValid = all (flip elem allowedChars) s

--
--Age validation functions
--

validateAge :: Int -> Maybe Int
validateAge age = nonNegative age <* notTooOld age

nonNegative :: Int -> Maybe Int
nonNegative age | age < 0   = Nothing
                | otherwise = Just age

notTooOld :: Int -> Maybe Int
notTooOld age | age > 150 = Nothing
              | otherwise = Just age

--
--Email validation functions
--

-- Overly simplified, and quite ugly.
-- A valid email address consists "local-part@domain" where "local-part" and
-- "domain"  may contain (a-z), (A-Z), (0-9) and periods.  
validateEmailAddress :: String -> Maybe String
validateEmailAddress email | matches   = Just email
                           | otherwise = Nothing
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


