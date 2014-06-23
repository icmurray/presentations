% Intro To Scala
% Ian Murray

## Overview

- Basics
    - Data Types
    - Functions
    - Pattern Matching
    - For comprehensions
- Types
- Standard Library

# The "Problem"

## Banking Transactions

Assume some service (database; REST api; etc) that provides the following
workflow:

~~~~ {.haskell}
-- Start a new transaction
mkTransaction :: User -> Password -> IO Transaction

-- Get the balance of a given account within the context of the transaction
getBalance :: Transaction -> Account -> IO Double

-- Credit/Debit a given account, and return the new balance.  Within the 
-- context of the given transaction
creditAccount :: Transaction -> Account -> Double -> IO Double

-- List available accounts
accounts :: Transaction -> IO [Account]

-- Commit/rollback the given transaction
commitTx :: Transaction -> IO Either (String, ())
rollbackTx :: Transaction -> IO ()
~~~~

## Banking Transactions

**Problem:** Build a layer _above_ this service which allows us to:

 - transfer money between two accounts
 - find all accounts with a negative balance
 - apply a fixed charge to all accounts with a negative balance.

_Within the context of a single transaction._

# Object-Oriented Scala

## This _may_ be a bit contrived...

\pause

~~~~ {include-scala=service.scala}
~~~~

## Traits

TODO: traits

## This _may_ be a bit contrived...

~~~~ {include-scala=service.scala}
~~~~

## Collections

TODO: collections

## This _may_ be a bit contrived...

~~~~ {include-scala=service.scala}
~~~~

## For Comprehension

TODO: for comps

## This _may_ be a bit contrived...

~~~~ {include-scala=service.scala}
~~~~

## Partial Function Application

TODO: partial function application

## An implementation...

\pause

~~~ {include-scala=impl.scala}
~~~

## Visibility modifiers

TODO: visibilty modifiers

## An implementation...

~~~ {include-scala=impl.scala}
~~~

## String formatting

TODO: string formatting

## Example usage...

\pause

~~~ {include-scala=usage.scala}
~~~

## Object Singleton

TODO: `object`

## Recap

 - traits, with partial implementations
 - `Unit`
 - for-comprehension
 - partial function application

\pause

 - `private[this]`
 - string formatting

\pause

 - object singleton
 - main entry point
