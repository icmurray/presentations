module PersonExamples where

import Control.Applicative
import qualified Data.Map as M

import Person

-- change this to ValidationEither etc...
import ValidationEither

-- Some examples of using mkPerson
    
workingMkPerson = mkPerson "A Person" 33 "a.person@example.com"
badUsername = mkPerson "" 33 "a.person@example.com"
badEmail = mkPerson "A Person" 33 "a.person@example.com@extra_domain.com"

badAgeAndEmail = mkPerson "A Person" 160 "a.person@"

mkPersonFromPrompt = mkPerson <$>            prompt "Username"
                              <*> fmap read (prompt  "Age")
                              <*>            prompt "Email"

mkPersonFromData m = mkPerson <$>            M.lookup   "username" m
                              <*> fmap read (M.lookup   "age"      m)
                              <*>            M.lookup   "email"    m
-- Some data in M.Maps

userData = M.fromList [("username", "a-person"),
                       ("age"     , "55"),
                       ("email"   , "a.person@example.com")]

missingUserData = M.fromList [("username", "a-person")]

invalidUserData = M.fromList [("username", "a-person"),
                              ("age"     , "-10"),
                              ("email"   , "a.person@")]


-- A useful prompt
prompt :: String -> IO String
prompt msg = putStr (msg ++ "> ") >> getLine

