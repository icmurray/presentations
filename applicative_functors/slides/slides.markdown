# An Introduction To Applicative Functors

---

# Overview

1. Functors.
2. A problem with Functors
3. The solution: Applicative Functors
4. A practical example. 
5. Defining Applicative Functors

---

# Preliminaries

## A recap of some useful types

* Maybe
* Either
* List
* IO

## A recap of type classes

* Defining them
* Declaring instances of them

## Introducing the Functor type class

* Definition
* Using the above types as Functors

## Presenter Notes

* easy to view these as containers, but can also be useful to think of these as
  a "computational context".  But these are just metaphors, and it's quite
  possible for an instance of a Functor to not fit either of these.
* all these types implement functor, and we'll show how using functor we can do
  things with the values within.

---

# Useful Data Type: Maybe

    !haskell
    data Maybe a = Nothing
                 | Just a

* As a container, `Maybe` represents a value that may or may not exist.

        !haskell
        -- For example, it's used here to represent the fact that a
        -- Person may not have an email address.
        data Person {
          name :: String,
          address :: String,
          email :: Maybe String
        }

* As a computational context, it represents a possible failure.

        !haskell
        -- For example, this is a safe version of the head function that
        -- fails gracefully when given an empty list.
        safeHead :: [a] -> Maybe a
        safeHead []     = Nothing
        safeHead (x:xs) = Just x

## Presenter Notes

* where some languages might use `null`, you would probably use a `Maybe` in Haskell.
* failure doesn't mean disk failure, or network problems; it's still pure.

---

# Useful Data Type: Either

    !haskell
    data Either a b = Left a
                    | Right b

- An `Either` type is a disjoint union of two types.
- By convention, it is right-biased.  We'll see what this means soon.
- As a container, it represents something that may take exactly one of two
  types.
- More commonly, as computational context, it represents a possible failure; but unlike
  `Maybe`, there's scope for returning information about that failure in the
  `Left` branch.

        !haskell
        -- For example, Parsec  uses `Either ParseError a` to provide an
        -- informative message should parsing fail.
        parseCSV :: String -> Either ParseError [[String]]
        parseCSV = undefined    -- we'll come back to this later...

---

# Useful Data Type: List

- We all know how a list behaves as a container.
- It can also be view as computational context by viewing it as a
  non-deterministic choice: that is, you can think of it as choosing a single
  value from a list of possibilities.

        !haskell
        diceRoll :: Int -> [Int]
        diceRoll n = [1..n]

---

# Useful Data Type: IO

In Haskell, any computation which deals with the outside world (ie - is not
pure) is represented by the `IO` type.  `IO a` is a computation which results
in a value of type `a`.

    !haskell
    readEmail :: IO String
    readEmail = putStrLn "What's your email address? " >> getLine

---

# An Aside: Type Constructors

All of the above are, strictly speaking, not types.  They are **type
constructors**.  That is, they take other types as parameters to produce a new
type.

* `Maybe`, `IO` and `[]` all take one parameter.
* Either takes two: `Either a b`
    * Due to currying, `Either err` is a type constructor, we'll see why this
      is important later.

---

# Functors

So far so good.  But how do I do anything with the values inside these data
types?  For example, if I have:

    !haskell

    import Data.List.Split (splitOn)  -- from the split package

    -- A value that may contain a String
    email :: Maybe String
    email = Just "a.person@example.com"

    -- A function for extracting the domain from an email address
    extractDomain :: String -> String
    extractDomain = last . splitOn "@"

How can I extract the domain from `email`?

---

# Functors

Pattern matching would work:

    !haskell
    -- Cumbersome, but it works...
    extractDomainFromMaybe :: Maybe String -> Maybe String
    extractDomainFromMaybe Nothing  = Nothing
    extractDomainFromMaybe (Just e) = Just (extractDomain e)

But it's not very pretty: I've had to write a new function just to be able to use
the already existing `extractDomain` function.

And it's not re-usable: I can't use `extractDomainFromMaybe` to allow me to use
`extractDomain` on other types, eg. `Either err String`, or `IO String`.  I'd
have to write two more functions

---

# Functors

Functors are the answer:

    !haskell
    -- extractDomainFromMaybe can now be re-written as
    extractDomainFromFunctor :: Functor f => f String -> f String
    extractDomainFromFunctor = fmap extractDomain

We'll look at how this works in a minute, but for now let's see what we can do
with it:

* We can extract a domain from a `Maybe String`.
* We can extract a domain from an `IO String`.
* We can extract domains from a `[String]`.

Also, it's worth pointing out that we needn't really have defined this function
at all:

* It's a one-liner.  Unlike `extractDomainFromMaybe` which uses
  pattern-matching.
* It's short enough to include in other functions.
* Sometimes using fmap as infix operator looks better.
* Functions can be composed together.

## Presenter Notes

Show in use in ghci:

    !haskell
    :load FunctorExamples.hs
    extractDomainFromFunctor email
    extractDomainFromFunctor readEmail   
    extractDomainFromFunctor ["a.person@example.com", "a.nother@gmail.com"]
    extractDomainFromFunctor Nothing :: Maybe String

    -- Without defining the function
    fmap extractDomain email

    -- fmap as infix
    extractDomain `fmap` email

    -- Composition
    -- Define a new function in ghci
    import Data.Char (toUpper)
    let toUpperString = map toUpper
    toUpperString "iaN"

    fmap (toUpperString . extractDomain) email

    -- or

    toUpperString `fmap` (extractDomain `fmap` email)

---

# Functors

    !haskell
    -- extractDomainFromMaybe can now be re-written as
    extractDomainFromFunctor :: Functor f => f String -> f String
    extractDomainFromFunctor = fmap extractDomain

There are two things worth looking at here:

* The signature of this function:

        !haskell

        extractDomainFromFunctor :: Functor f => f String -> f String
        --                         |____________|
        --                                |
        --                              This looks different.

* What is `fmap`?

    * A function that all `Functors` must implement.

---

# Functors: Definition

So what is a Functor anyway?

    !haskell
    class Functor f where
      fmap :: (a -> b) -> f a -> f b

* Type class that declares the instances of `Functor` must implement `fmap`.
* The type signature of `fmap` shows us that `f` is a **type constructor** with a single parameter.
    * `instance Functor String` makes no sense because `String` is not a type
      constructor.
    * `instance Functor Maybe` works because `Maybe` is a type constructor with
      a single parameter.
    * `instance Functor Either` makes not sense because `Either` is a type
      constructor with **two** parameters.
    * `instance Functor (Either a)` works because `Either a` is a type
      constructor with a single parameter.

---

# Functors: fmap

There are two ways to think about `fmap`:

1. `fmap` takes two parameters: a function and a container.  And it returns a
   new container with exactly the same structure , but the value(s) inside have
   had a function applied to them.

2. `fmap` is a function that takes one parameter, and returns another function:

        !haskell
        fmap :: (a -> b) -> f a -> f b

        -- or, equivelantly:

        fmap :: (a -> b) -> (f a -> f b)

   In this view, `fmap` takes a *normal* function and transforms it into a
   function that can operate on Functors.  This is called **lifting**.

---

# Functors: Functor Laws

Not represented within the definition of `Functor` are the *functor laws*.
These serve to ensure that the structure of the container is not changed by
using `fmap`.

    !haskell
    fmap id = id
    fmap (g . h) = (fmap g) . (fmap h)

---

#  Functors: Defining instances

Let's have a go at defining our own instances of `Functor` for `Maybe` and
`Either`.

## Presenter Notes

Edit FunctorExamples.hs

---

# Functors: The problem with Functors

Functors have a problem.  Let's say I have a function that takes two parameters:

    !haskell
    buildEmail :: String -> String -> String
    buildEmail name domain = name ++ "@" ++ domain

How can I use that function with two `Maybe String`s: `Just "a.person"` and
`Just "example.com"`.

Let's try *lifting* buildEmail...

---

# Lifting buildEmail

First, a reminder of `fmap`'s type:

    !haskell
    fmap :: Functor f => (a -> b) -> f a -> f b

So we need to fit `buildEmails`'s type signature into `(a -> b)`.  To help see
this, we can re-parenthesise it's signature:

    !haskell
    buildEmail :: String -> (String -> String)

So, `a` is `String`, and `b` is `String -> String`:

    !haskell
    fmap buildEmail :: Functor f => f String -> f (String -> String)

Let's keep going, this might work out...  I've got an `f String`, so let's add
it to the argument list:

    !haskell
    fmap buildEmail (Just "a.person") :: Maybe (String -> String)

So I've ended up with a function wrapped in a Maybe!

---

# ... continued

What can I do with a function wrapped in a `Maybe`?  Well, it's a `Functor`, so
let's `fmap` something over it...

    !haskell
    fmap something (fmap buildEmail (Just "a.person"))

But what should `something` be?  I know it's a function...

    !haskell
    something = \f -> somethingElse

And I know `f` is itself a function of type `String -> String`, because that's
what's inside the `Maybe`.  Well, how about handing it a `String`?

    !haskell
    something = \f -> f "example.com"

Trying the whole thing out...

    !haskell
    fmap (\f -> f "example.com") (fmap buildEmail (Just "a.person"))
    Just "a.person@example.com"

Great! That looks ok, it's the *answer* we wanted.  But there's been a bit of
sleight of hand...

---

# ... continued

We've had to unwrap `Just "example.com"` in order to define `something`.  This
is what we wanted to avoid.

---

# Applicative Functors

This is exactly the problem that applicative functors solve.

Before going through the details of how they work, let's look at some examples:

    !haskell
    buildEmail <$> Just "a.person" <*> Just "example.com"
    Just "a.person@example.com"

    buildEmail <$> Just "a.person" <*> Nothing
    Nothing

    buildEmail <$> Nothing <*> Just "example.com"
    Nothing

    buildEmail <$> Nothing <*> Nothing
    Nothing

This in fact works for any number of arguments, in general:

    !haskell
    function <$> arg1 <*> arg2 <*> ... <*> argN

Returns the application of function to the N given arguments, with the result
wrapped up in an `Applicative`.

Where function is an N-argument function, and the arguments are all instances
of the **same** `Applicative`.

Note the first operator is `<$>`, and the rest are `<*>`.  We'll soon see what
these functions do.

## Presenter Notes

    ghci
    :load code/ApplicativeExamples.hs
    
    -- IO
    buildEmail <$> prompt "name" <*> prompt "domain"

    -- Not all the arguments to the function need to be the same type
    Person <$> promptName <*> promptAge <*> promptEmail

    -- There are a couple of other functions:
    Just 45 <* Just 50
    Just 45

    Just 45 *> Just 50 *> Just 60
    Just 60

    Just 45 <* Nothing <* Just 60
    Nothing

---

# Validation with Applicative

Lets say we have the following data type for representing a Person:

    !haskell
    type Email = String  -- just a synonym
    
    data Person = Person {
      username :: String,
      age :: Int,
      email :: Email
    } deriving (Show, Eq)

And we have some validation rules for each of the fields:

* username must not be blank
* username must be stripped of leading and trailing whitespace
* username may only contain ascii-alpha-numerics or hypens, underscores or
  spaces.
* age must be positive
* age must be less-than-or-equal 150
* email must *appear* to be a valid email address

## Presenter Notes

Want to try to build an intuition for how applicatives are used before showing
how they work under the hood.  Using validation as a motivation for this.

---

# Validation: Maybe

As a first attempt, we'll use the Maybe Applicative.

See code/ValidationMaybe.hs

    !haskell
    mkPerson "A Person" 33 "a.person@example.com"
    Just (Person {username = "A Person", age = 33, email = "a.person@example.com"})

    mkPerson "" 33 "a.person@example.com"
    Nothing

    mkPerson "A Person" 33 "a.person@example.com@extra_domain.com"
    Nothing

## Presenter Notes

    -- Also, mkPerson itself can be lifted:

    -- perhaps the data comes directly from user input
    mkPerson <$>            prompt "Username"
             <*> fmap read (prompt  "Age")
             <*>            prompt "Email"

    -- or maybe it comes from a Map that's been read from somewhere else,
    -- and may not have all the inputs
    mkPerson <$>            M.lookup   "username" userData
             <*> fmap read (M.lookup   "age"      userData)
             <*>            M.lookup   "email"    userData

---

# Validation Either

As a second attempt, we'll use the Either Applicative

see code/ValidationEither.hs
    
    !haskell
    mkPerson "A Person" 33 "a.person@example.com"
    Right (Person {username = "A Person", age = 33, email = "a.person@example.com"})

    mkPerson "" 33 "a.person@example.com"
    Left "Username is blank"

    mkPerson "A Person" 33 "a.person@example.com@extra_domain.com"
    Left "Invalid email address"

---

# Improving on Either

`Either String Person` was a definite improvement over `Maybe Person` because
we get some feedback as to what was invalidated, but it still leaves room for
improvement:

1.  If two arguments are invalid, we only get an error message about the first.

        !haskell
        mkPerson "A Person" 160 "a.person@"
        Left "Too old"

2.  The helper functions are quite useful, but they're not very re-usable.
    Take `nonNegative` for example, that could be applied to other `Integer`
    arguments, but the error message is tied to the fact that it's being used to
    validate an age.

        !haskell
        nonNegative :: Int -> Either String Int
        nonNegative age | age < 0   = Left "Negative age!"
                        | otherwise = Right age

Before we see how we could improve on this situation, we're going to look at
how `Applicative` works, and how we can define our own `Applicative` instances.

---

# Applicative Functors: Definition

An Applicative is defined by the following typeclass

    !haskell
    class Functor f => Applicative f where
      pure :: a -> fa
      (<*>) :: f (a -> b) -> f a -> f b
      (*>) :: f a -> f b -> f b
      (<*) :: f a -> f b -> f b

Only `pure` and `<*>` need to be defined by an instance.

* `<*>` gives us a way of doing exactly what a `Functor` couldn't: applying a
  function residing within a context to a value also residing within a context.

* `pure` gives us a way of putting a value into a context.  This is necessary
  because each other function above acts on things already embedded within a
  context.

* Also note that every `Applicative` must be a `Functor`.  Mathematically,
  every `Applicative` already is a `Functor` (if it abides by the applicative
  functor laws), ie we can define `fmap` in terms of `pure` and `<*>`.  But this
  formalises this relation in the type system.

---

# Applicative: Relation to Functor

Seeing `pure` and `<*>`'s relation to `fmap` helps build an intuition as to how an `Applicative` should behave:

    !haskell
    fmap g x = pure g <*> x

In other words, `fmap`-ing a function, `g`, over a context x is the same as
using `pure` to embed `g` within a context, and then using `<*>` to apply `g`
to the value within the context x.

---

# Applicative: Relation to Functor

    !haskell
    -- So, previsouly we've used fmap to extract a domain from an email
    extractDomain :: String -> String
    extractDomain = last . splitOn "@"

    -- ie, we did:
    fmap extractDomain email

    -- using
    fmap g x = pure g <*> x

    -- we see this can also be written as
    pure extractDomain <*> email

Last piece of the puzzle is:

    !haskell
    -- The definition of <$>
    (<$>) :: Functor f => (a -> b) -> f a -> f b
    (<$>) g x = pure g <*> x

    -- Meaning we can write
    extractDomain <$> email

---

# Applicative Functor: Multiple Arguments

So we're now in a position to see how `Applicative`s work with functions that
take more than argument:

    !haskell
    buildEmail :: String -> String -> String
    buildEmail name domain = name ++ "@" ++ domain

    -- We now know that this works:
    buildEmail <$> Just "a.person" <*> Just "example.com"
    Just "a.person@example.com"

Let's parenthesis the above, so we can see what's going on

    !haskell
    (buildEmail <$> Just "a.person") <*> Just "example.com"

and just look at the value of the first expression

    !haskell
    buildEmail <$> Just "a.person"

---

# ...continued

    !haskell
    buildEmail <$> Just "a.person"

from the definition of `<$>`, this is equivelant to

    !haskell
    -- (<$>) g x = pure g <*> x
    pure buildEmail <*> Just "a.person"

from relationship with fmap, this is equivelant to

    !haskell
    -- fmap g x = pure g <*> x
    fmap buildEmail (Just "a.person")

from the type signature of buildEmail, we see the type is:

    !haskell
    fmap buildEmail (Just "a.person") :: Maybe (String -> String)

---

# ...continued

This is where we got stuck last time: we had a function wrapped in a `Maybe`.
But this is exactly what `<*>` can handle:

    !haskell
    (<*>) :: f (a -> b) -> f a -> f b

    -- back in the original expression:

    (buildEmail <$> Just "a.person") <*> Just "example.com"

    -- with pure g = (buildEmail <$> Just "a.person") :: Maybe (String ->  String)
    -- and       x = Just "example.com"

At this stage we'd have a look at how the `Maybe` instance of `Applicative` is
defined, but since we haven't defined that yet, I'm appealing to your sense of
how it should work.  ie - `pure g` is either `Nothing`, or it's a `Just`
containing a function.

---

# ..cont.

So, assuming that `pure g` is not `Nothing`, then applying the relation to
fmap again:

    !haskell
    -- reminder: fmap g x = pure g <*> x
    -- if pure g == Nothing, then there'd be nothing to extract from pure g
    fmap g (Just "example.com")

    -- equivelant to
    fmap (buildEmail "a.person") (Just "example.com")
    Just "a.person@example.com"

---

# Applicative: Defining instances

Let's define Applicative instances for Maybe and Either.

## Presenter Notes

edit ApplicativeExamples.hs

---

# Improving upon validation

One problem with our use of `Either String` in validation was that only the
first failing validation rule was returned upon error.  We'd like to receive
**all** broken validation rules.

    !haskell
    mkPerson "A Person" 160 "a.person@"
    Left "Too old"

To do this, we're going to define our own data type, very much like `Either`,
but we're going to accumulate errors.

## Presenter Notes

Edit ValidationAccumulate.hs

mkPerson "A Person" 33 "a.person@example.com"
mkPerson "" 33 "a.person@example.com"
mkPerson "A Person" 160 "a.person@"

---

# Generalising ValidationAcc

The `ValidationAcc` data type can be generalised further using a data type
called a `Monoid`.  **Very briefly**, this is a `Monoid`:

    !haskell
    class Monoid a where
      mempty  :: a
      mappend :: a -> a -> a
 
      mconcat :: [a] -> a
      mconcat = foldr mappend mempty

A Monoid is a set of elements, and an operator, `mappend` over those elements.
Within that set of elements is the `mzero` element, which forms the identity
under `mappend`.

---

# A brief detour in Monoids

Lots of things are Monoids:

* Integers under addition. (Data.Monoid.Sum)
* Integers under multiplication. (Data.Monoid.Product)
* Lists
* Maybe is a Monoid if the contained type is a Monoid.
* Tuples are Monoids if the contained types are Monoids.
* Maps are monoids
* There are lots more...

## Presenter Notes

In ghci

import Data.Monoid
import qualified Data.Map as M

Sum 45 `mappend` Sum 90
Product 100 `mappend` Product 7
[1,2,3] `mappend` [5,6,7]
Just [1,2,3] `mappend` Nothing
Just [1,2,3] `mappend` Just [4,5,6]

let m1 = M.fromList [("key1", "value1"), ("key2", "value2")]
let m2 = M.fromList [("key2", "value2"), ("key3", "value3")]
m1 `mappend` m2   -- notice keeps LHS if keys clash.

---

# Monoids continued...

We can write our own version of the Map Monoid to accumulate values rather than
keep only items in the left hand side (when keys clash).

To do this, we need to define a new data type.

    !haskell
    newtype AccumulatingMap k v = AccMap { toMap :: Map k v }

`newtype` is a third way of declaring types: it acts as completely new type
which is implemented as wrapper around an existing type.

---

# Accumulating Map continued...

Now to define it's Monoid instance:

    !haskell
    instance (Ord k, Monoid v) => Monoid (AccumulatingMap k v) where
      mempty = AccMap M.empty
      AccMap m1 `mappend` AccMap m2 = AccMap (M.unionWith mappend m1 m2)

---

# Generalised Validation

Let's re-write`ValidationAcc` to accumulate failures in a monoid, rather
than a list of values.

## Presenter Notes

Edit ValidationRich.hs

mkPerson "A Person" 33 "a.person@example.com"
mkPerson "" 33 "a.person@example.com"
mkPerson "A Person" 160 "a.person@"

---

# Generalised Validation: ...continued

Gives us same behaviour for `[String]` as before.  However, we could try other monoids:

* Counting the number of validation errors

        !haskell
        mkPerson <$> Failure (Sum 1)
                 <*> Success 33
                 <*> Failure (Sum 1)

* Accumulating errors in a Map

        !haskell
        mkPerson <$> Failure (M.singleton "username" "Blank")
                 <*> Success 33
                 <*> Failure (M.singleton "email" "Bad domain")

* Accumulating errors in a AccMap

        !haskell
        mkPerson <$> Failure (AccMap (M.singleton "username" ["Blank"]))
                 <*> Success 33
                 <*> Failure (AccMap (M.singleton "email" ["Bad local-part", "Bad domain"]))

In fact, accumulating errors in a Map would be a good way of allowing use
re-use of the private validation functions, such as `nonNegative`, outside of age
validation.

## Presenter Notes

Edit ValidationRich.hs
Show enrich function at bottom
Add `enrich` to mkPerson

---

# More applicative style programming

What we've seen so far is an example of applictive style programming.  It also
crops up in one of Haskell's json libraries,
[aeson](http://hackage.haskell.org/packages/archive/aeson/latest/doc/html/Data-Aeson.html):

    !haskell
    -- taken from aeson's docs
    {-# LANGUAGE OverloadedStrings #-}

    data Coord { x :: Double, y :: Double }
    
    instance FromJSON Coord where
       parseJSON (Object v) = Coord    <$>
                              v .: "x" <*>
                              v .: "y"
    
    -- A non-Object value is of the wrong type, so use mzero to fail.
       parseJSON _          = mzero

---

# More applicative style programming

There's also an applicative interface to Parsec, the parser combinator library.

    !haskell
    -- An example adapted from real-world haskell
    p_query :: CharParser () [(String, Maybe String)]
    p_query = pair `sepBy` char '&'
      where pair = (,) <$> (many1 safe)
                       <*> (optional (char '=' *> many safe))
            safe = oneOf urlBaseChars
               <|> char '%' *> (diddle <$> hexDigit <*> hexDigit)
               <|> ' ' <$ char '+'
            diddle a b = toEnum . fst . head . readHex $ [a,b]
    
    urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"
    

Compare the `pair` function to its monadic-style counterpart:

    !haskell
    p_pair :: CharParser () (String, Maybe String)
    p_pair = do
      name <- many1 safe
      value <- optionMaybe (char '=' >> many safe)
      return (name, value)

---

# What I haven't talked about

* The original applicative functor [paper](http://www.soi.city.ac.uk/~ross/papers/Applicative.html).

  * Motivation of the abstraction is different than presented here.

* Composition of Applicative is possible in the general case:

  * Given two Applicatives, `A` and `B`, then `A ( B a )` is also an Applicative.

* Applicatives that aren't Monads

  * `ZipList` is the canonical example.
  * `Data.Applicative.Const` is another.

* Limitations of Applicatives

  * Actions are independant of each other.
    * All actions are run.
    * Can't pass values from one into the next.

---

# References

* [Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia)
* [HaskellWiki](http://en.wikibooks.org/wiki/Haskell/Applicative_Functors)
* [Real World Haskell](http://book.realworldhaskell.org/read/using-parsec.html)
* [Bryan O'Sullivan's blog](http://www.serpentine.com/blog/2008/02/06/the-basics-of-applicative-functors-put-to-practical-work/)
* [Learn you a haskell for great good](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors)
* Tony Morris' [validation presentation](http://dl.dropbox.com/u/7810909/docs/applicative-errors-scala/applicative-errors-scala/pdf/index.pdf) (in Scala).
* scalaz [Validation class](https://github.com/scalaz/scalaz/blob/master/core/src/main/scala/scalaz/Validation.scala)
* Tony Morris' [Validation package for Haskell](http://hackage.haskell.org/package/Validation-0.2.0)
