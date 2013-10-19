% Tour Of Scala
% Ian Murray

## Overview

- TODO
- TODO

# Simple Build Tool

## SBT

- Scala-based configuration
- Continuous compilation and testing
- Dependency management
- Extendable via plugins
- Package & publish jars
- Generate documentation

## A HelloWorld Configuration File

Contents of `build.sbt`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    name := "hello"

    version := "1.0"

    scalaVersion := "2.10.3"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Scala-based configuration

Contents of `project/Build.scala`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import sbt._
    import Keys._

    object BiddingExample extends Build {

      lazy val globalSettings = Defaults.defaultSettings ++ Seq(
        scalaVersion in ThisBuild := "2.10.3"
      )

      lazy val hello = Project(id        = "hello",
                               base      = file("."))
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Managed Dependencies

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val globalSettings = Defaults.defaultSettings ++ Seq(
      scalaVersion in ThisBuild := "2.10.3",
      libraryDependencies ++= Seq(
         "joda-time"      %  "joda-time"    % "2.2",
         "org.joda"       %  "joda-convert" % "1.3.1",
         "org.scalatest"  %% "scalatest"    % "1.9.2"   % "test")
    )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Managed Dependencies

The `%%` adds the scala version to the artifact, ie

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
     "org.scalatest"  %% "scalatest"           % "1.9.2"   % "test")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

becomes:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
     "org.scalatest"  % "scalatest_2.10.3"     % "1.9.2"   % "test")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some dependencies are compiled for different versions of Scala, this is a
convenience for picking the version that matches your project.

Different versions of Scala can be binary incompatible (but maintain source
compatibility).  SBT can help you publish your library against multiple
versions of Scala.

## Managed Dependencies

### Adding repositories

SBT uses the standard Maven2 Repository by default.  If your dependency is not
available there, then you can add further repositories:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val globalSettings = Defaults.defaultSettings ++ Seq(
      scalaVersion in ThisBuild := "2.10.3",
      libraryDependencies ++= Seq(
         "joda-time"      %  "joda-time"    % "2.2",
         "org.joda"       %  "joda-convert" % "1.3.1",
         "org.scalatest"  %% "scalatest"    % "1.9.2"   % "test"),
      resolvers ++= Seq(
        "Spray Repo" at "http://repo.spray.io/")

    )
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Directory Structure

### Sources

Follows Maven's:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    src/
      main/
        resources/
           <files to include in main jar here>
        scala/
           <main Scala sources>
      test/
        resources
           <files to include in test jar here>
        scala/
           <test Scala sources>
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Directory Structure

### Build Defintion(s)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    build.sbt
    project/
      Build.scala
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Artificats

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    target/
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## SBT Console

- REPL-like console
- execute tasks
    - `compile`
    - `run`
    - `test`
- drop into the Scala REPL
    - `console`

## SBT Console

### Compile

SBT will only compile source files that have changed since the previous
compilation, and those sources which depend on them.

Reduces compilation time.

## SBT Console

### Testing

> - `specs2`, `ScalaCheck` and `ScalaTest` runners can all be run within SBT, just
by executing the `test` task.

> - `test-only` can be used to selectively run particular tests
      - `test-only org.example.Test1 org.example.*Slow`

> - `test-quick` will only run tests which:
     - have failed in the previous run, or
     - were not run previously, or
     - tests which have had dependencies recompiled

## SBT Console

### Scala REPL

- Drop into the Scala console with the `console` task
- Or, if your project isn't compiling, drop into the Scala console with
  `console-quick`.

## SBT Console

### Triggered Execution

Any task can be prefixed by the `~` character.  SBT will monitor source files
and re-run the task 

Eg.

> - `~compile` will continually and incrementally re-compile your project
     when a source file is saved.

> - `~test-quick` will continually run the parts of your test-suite that have
     been affacted by your code changes.

## Sub-projects

Splitting projects can be useful, let's give our hello world application a REST
API and a command line interface.

- `core` project, defining models, business logic etc.  Acts as a library.
- `api` project, defining the REST API, which depends on `core`.
- `cli` project which also depend on `core`.
- `root` umbrealla project

## Sub-projects

1. Create a `root` project which aggregates everything together:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val root      = Project(id        = "hello",
                                 base      = file("."),
                                 aggregate = Seq(core, cli, api))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sub-projects

2. Create a `core` project which defines its own dependencies:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val core = Project(
      id        = "hello-core",
      base      = file("core"),
      settings  = globalSettings ++ Seq(
        libraryDependencies ++= Seq(
          "joda-time"      %  "joda-time"    % "2.2",
          "org.joda"       %  "joda-convert" % "1.3.1")
      ))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sub-projects

3. The `cli` project has no extra dependencies other than `core`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val cli = Project(
      id   = "hello-cli",
      base = file("cli")) dependsOn(core)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sub-projects

4. The `api` project requires some extra libraries:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    lazy val api = Project(
      id        = "hello-api",
      base      = file("api"),
      settings  = globalSettings ++ Seq(
        libraryDependencies ++= {
          val sprayVersion = "1.2-M8"
          val akkaVersion  = "2.2.0"
          Seq(
            "io.spray"            %   "spray-can"     % sprayVersion,
            "io.spray"            %   "spray-routing" % sprayVersion,
            "io.spray"            %   "spray-testkit" % sprayVersion,
            "com.typesafe.akka"   %%  "akka-actor"    % akkaVersion)
        }
      )) dependsOn(core)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Sub-projects

### Using sub-projects

- Each sub-project will be packaged into its own JAR.

- In the SBT console, you can work in the context of a single sub-project
  *or* the `root` umbrella project.

# Cake Pattern

## Cake Pattern

### The Problem

> - Let's say we want some component for creating users and authenticating them.
> - We want to abstract over the mechanism for persisting users.
> - Which in turn may abstract over some things of its own.

## Cake Pattern

### Setting the scene

Contents of `users.scala`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    case class User(
        id: UserId,
        name: String,
        password: HashedPassword)

    case class UserId(value: Long) extends AnyVal
    case class HashedPassword(value: String) extends AnyVal
    case class PlaintextPassword(value: String) extends AnyVal
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Setting the scene

Contents of `users.scala`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait UserService {

      def create(username: String,
                 password: PlaintextPassword): Try[User]

      def authenticate(username: String,
                       password: PlaintextPassword): Option[User]
                       
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Setting the scene

Contents of `dao.scala`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait UserDAO {

      def insert(username: String,
                 password: HashedPassword): Try[User]

      def byUsername(username: String): Option[User]
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Setting the scene

Now we can create a concrete implementation of our UserService that uses a
given `UserDAO`

## Cake Pattern

### Setting the scene

Contents of `users.scala`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class PersistantUserService(dao: UserDAO) extends UserService {

      def create(username: String, plaintext: PlaintextPassword) = {
        val hashed = hashPassword(plaintext)
        dao.insert(username, hashed)
      }

      def authenticate(username: String, plaintext: PlaintextPassword) = {
        for {
          user <- dao.byUsername(username)
          if passwordsMatch(plaintext, user.password)
        } yield user
      }

      private def hashPassword(plaintext: PlaintextPassword)
                              : HashedPassword = ???

      private def passwordsMatch(plaintext: PlaintextPassword,
                                 hashed: HashedPassword): Boolean = ???

    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### The Problem

> - *Somewhere*, a piece of code is responsible for creating a new
    `PersistantUserService`.
> - That means it needs to provide a concrete `UserDAO` implementation as well.
> - Which, in turn, will have its own dependencies to pass-in to the
    constructor.

## Cake Pattern

### The Problem

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val userDao: UserDAO = new PostgresUserDAO( ... )
    val userService: UserService = new PersistantUserService(userDAO)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Extrapolate this across a larger code-base, and we have a reason to use Spring.

Right?

## Cake Pattern

### Enter The Cake Pattern...

Traditional dependency injection frameworks will, at runtime, populate your
instance's dependencies based upon the configuration found in an `XML` file.

What could possibly go wrong?!

Using the Cake Pattern is one way ensure that your dependencies are fulfilled
**at compile time**, and does not require any extra dependencies or injection
framework.

It can however appear a little warped...

## Cake Pattern

### Step 1

Add a layer of indirection...

## Cake Pattern

### Step 1

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait UserServiceModule {

      val userService: UserService

      trait UserService {
        def create(username: String,
                   password: PlaintextPassword): Try[User]

        def authenticate(username: String,
                         password: PlaintextPassword): Option[User]
      }

    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

- The `UserServiceModule` defines **what** a `UserService` is,
- ... and **how** we can get one.

## Cake Pattern

### Step 2

Repeat the same for our `UserDAOModule`...

## Cake Pattern

### Step 2

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait UserDAOModule {

      val userDAO: UserDAO

      trait UserDAO {
        def insert(username: String,
                   password: HashedPassword): Try[User]

        def byUsername(username: String): Option[User]
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Step 3

Define a concrete implmentation of our **`UserDAOModule`** which uses a
database backend.

## Cake Pattern

### Step 3

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
trait PostgresUserDAOModule extends UserDAOModule {

  val connectionPool: ConnectionPool
  val userDAO = new Impl()

  class Impl extends UserDAO {

    // Concrete implementations...
    def insert(username: String,
               password: HashedPassword) = ???
    def byUsername(username: String) = ???
    }
  }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

> - The module itself **extends** `UserDaoModule`, ie it defines **how** to
    access a `UserDAO` instance.
> - The `Impl` class provides the actual implementation of `UserDAO`.
> - The module itself is still abstract, so we can defer things we don't know
    about, in this case, the `connectionPool`.  (Although, you'd consider
    creating a `ConnectionPoolModule` or similar in this case).

## Cake Pattern

### Step 4

Define a concrete implementation of our **`UserServiceModule`** which delegates
to a `UserDAO`.

## Cake Pattern

### Step 4

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
trait PersistedUserServiceModule extends UserServiceModule {
  this: UserDAOModule =>    // declare dependency on UserDAOModule

  val userService = new Impl()

  class Impl extends UserService {
    def create(username: String,
               plaintext: PlaintextPassword) = {
      val hashed = hashPassword(plaintext)
      userDAO.insert(username, hashed)
    }

    def authenticate(username: String, plaintext: PlaintextPassword) = {
      for {
        user <- userDAO.byUsername(username)
        if passwordsMatch(plaintext, user.password)
      } yield user
    }

    // Helper methods ellided
  }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Self-types

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait PersistedUserServiceModule extends UserServiceModule {
      this: UserDAOModule =>    // declare dependency on UserDAOModule
    
      // rest of trait ...
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> - `this: UserDAOModule` means "When I'm mixed in to a concrete class, that
    concrete class must also implement the `UserDAOModule` trait.
> - In the Cake Pattern context, it signals a dependency.
> - It's what allows us to use `userDAO` in the `Impl` class.

## Cake Pattern

### Self-types

Sometime inheritance is used instead, ie:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait PersistedUserServiceModule extends UserServiceModule
                                        with UserDAOModule {
      // rest of trait ...
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

But self-types:

- Separate *role* from *dependencies*.
- Allow for circular references.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait A { this: B => }
    trait B { this: A => }    // compiles

    trait A extends B
    trait B extends A         // won't compile
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Step 5

Pulling it all together at the end of the world...  (baking the cake)


## Cake Pattern

### Step 5

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    object Modules extends PersistedUserServiceModule
                      with PostgresUserDAOModule {

      // Probably makes more sense to declare dependency on a
      // ConnectionPoolModule, but this illustrates some of the flexibility.
      val connectionPool: ConnectionPool = ???
    }

    Modules.userService.create("ian", PlaintextPassword("password"))
                              
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

### What have we achieved?

## Cake Pattern

### Type safety

It's impossible to forget to mix in a required ingredient, eg. this won't
compile:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    object Modules extends PersistedUserServiceModule
                      with PostgresUserDAOModule {
      // Connection pool missing.
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

Neither will this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    object Modules extends PersistedUserServiceModule {
      val connectionPool: ConnectionPool = ???
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Cake Pattern

### Loosely coupled

- There's nothing that says a `PersistantUserService` must use a **particular**
  `UserDAO` implementation.

- There's nothing that says a `UserService` must use a `UserDAO` **at all**.

## Cake Pattern

### Testable

We can wire up a mock `UserDAO` to a `PersistedServiceModule`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}

    trait MockUserDAOModule extends UserDAOModule {
      val userDAO = mock[UserDAO]
    }

    object TestEnv extends PersistedUserServiceModule
                      with MockUserDAOModule

    // ... or even

    object TestEnv extends UserDAOModule
                      with PersistedUserServiceModule {
      val userDAO = mock[UserDAO]
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Cake Pattern

### Cons

> - Can't be used at runtime to configure components.
> - Quite verbose.
    - Not only the nested definitions, but the *naming* too.
> - Can run into name collisions.
> - Trait inheritance can get hairy
      - Order matters when using `abstract override def`s.
      - Instantiation order can cause `NullPointerException`s.
          - Can mostly be mitigated by using `lazy val`s.

