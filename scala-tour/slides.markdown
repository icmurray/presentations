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

# Slick

## Slick: Scala Language-Integrated Collection Kit

> A modern database query and access library for Scala.

## Slick

> - Query for data in a similar way to how you work with scala collections.
> - Write queries in Scala instead of SQL.
> - Static checking of queries.
> - Database access is kept explicit, not hidden away.

## Slick

### Overview

There's a **lot** to Slick, and I'm not going to attempt to cover all of it
here.  This is just a taster really, and Slick would easily warrant a talk of
its own.

> - Querying using Slick's *Lifted Embedding* API.
> - Table definitions.
> - Some practicalities

\pause

I *won't* be covering:

> - Aggregation.
> - Examples of different joins and zipping.
> - Query templates.
> - Raw SQL
> - User defined functions.
> - Direct embedding API.
> - Anything under the hood.

## Slick

### Querying

> - Slick has a `Query[+E, U]` type which models a SQL query.
> - `E` represents the type of the *supplier* of the data that is being
>   queried, eg.  `(Column[String], Column[Long])`.
> - `U` represents the type of the data that you get back when the query is
>   run, eg. `(String,Long)`.

## Slick

### Querying

With for-comprehensions:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    for {
      user <- Query(Users)
      if user.username.toLowerCase like "frank"
    } yield user.id
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

Or with method calls:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    Query(Users).filter(_.username.toLowerCase like "frank")
                .map(_.id)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

> - The database is not accessed in the above.  It is merely a transformation
    of `Query` objects.
> - Things like `toLowerCase` are methods made implicitly available on
>   `Column[String]` which result in the required SQL being constructed - ie.
>   they are not functions that act on String instances running in Scala.

## Slick

### Querying

Cross-joins are created when `flatMap`-ing across multiple tables/queries:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    for {
      user1 <- Query(Users)
      user2 <- Query(Users)
      if user1.username.toLowerCase === user2.user.toLowerCase
    } yield (user1.id, user2.id)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- Also, note the triple `=` equality.  This is required for checking for
  equality.  Other operators (`>`, `>=`) work as you'd expect.

## Slick

### Querying

The idea is that `Query`s are composable and re-usable:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def nameLike[E](users: Query[UsersTable,E], pattern: Column[String]) = {
      users.filter { _.username.toLowerCase like pattern }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

- `nameLike` accepts a `Query` over the Users table as its first argument:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val q1 = Query(Users).filter(_.id > 10)
    nameLike(q, "frank")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

- In this case, `"frank"` is implicitely converted from a `String` to a
  `Rep[String]`, but we could use an actual database column there too:

\pause

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    for {
      user1 <- Query(Users)
      user2 <- nameLike(Query(Users), user1.username)
    } yield (user1, user2)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Querying

To actually execute any of these queries, access to a `Session` is required.
The simplest way to get hold of one is:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    // Import the slick driver for the particular database
    import scala.slick.driver.H2Driver.simple._

    // Instantiate a Database
    val db = Database.forURL("jdbc:h2:mem:test1", driver = "org.h2.Driver")

    // Have slick manage the Session for you...
    db.withSession { implicit s: Session => /* Execute Query(s) */ }

    // ... alternatively, within a transaction
    db.withTransaction { implicit s: Session => /* Execute Query(s) */ }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

There are a couple of things going on here:

> 1. There's the usual loading of the correct JDBC driver class for the
>    database you want to connect to.
> 2. In order for Slick to be able to generate the SQL code specific to your
>    database engine, it needs to load it's own drivers for a particular
>    database.

## Slick

### Querying

Once you have a `Query` and a `Session`, then you can execute the `Query`, and
access the data in a number of ways:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val q = nameLike(Query(Users), "frank")
    val qPairs = q.map { user => (user.id, user) }
    
    db.withSession {
      // Reading a complete result set into various collections
      val franks:   List[User]        = q.list
      val franks2:  List[User]        = q.to[List]
      val frankSet: Set[User]         = q.to[Set]
      val frankMap: Map[UserId, User] = qPairs.toMap
      val frank:    User              = q.first
      val frankO:   Option[User]      = q.firstOption
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Querying

You can also consume the resultset lazily:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    db.withSession {
      // Folding over the result
      q.foldLeft(""){_ + _.name}: String

      // Perform side-effect for row
      q.foreach(println)

      // Manually iterating over the result
      val iter: CloseableIterator[User] = q.elements
      try {
        iter.foreach(println)
      } finally { iter.close() }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Updating

It's not just about querying for data either, `Query`s can be used to update
data too:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val theOnlyFrank = Query(Users).filter(_.username === "frank")
                                   .map(_.username)
    
    db.withSession { implicit s: Session =>
      theOnlyFrank.update("frank sinatra")
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

There are some limitations:

- The `Query` (`theOnlyFrank`) must return raw columns selected from a single
  table.
- There is (currently) no way to **transform** the existing data, ie - the
  update method expects a single scalar value.

## Slick

### Deleting

And as you'd expect, *deleting* data works too:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val theOnlyFrank = Query(Users).filter(_.username === "frank")
    
    db.withSession { implicit s: Session =>
      theOnlyFrank.delete
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Inserting

We'll come to this shortly after we've looked at defining table definitions.

## Slick

### Table Definitions

Tables are declared in Scala, and can optionally be mapped to domain objects.

## Slick

### Table Definitions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    // Slick driver required for table definition.
    import scala.slick.driver.H2Driver.simple._

    class UsersTable extends Table[(Long, String, String)]("users") {
      def id       = column[Long]("id", O.PrimaryKey, O.AutoInc)
      def username = column[String]("username")
      def password = column[String]("password")

      def * = id ~ username ~ password

      def usernameIdx = index("idx__users__username", username, unique = true)
    }

    val Users = new UsersTable()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

A few things to note:

> 1. The types of the columns don't reflect the types used in the `User` model.
> 2. We're reading `(Long,String,String)`s from the table, not `User`s.
> 3. The table definition is tied to the Slick H2 driver

## Slick

### Table Definitions

Improve first definition by storing `UserId`s and `HashedPassword`s:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    implicit def userIdTypeMapper = MappedTypeMapper.base[UserId, Long] (
      userId => userId.value,
      long   => UserId(long)
    )

    implicit def hashedPasswordTypeMapper = {
      MappedTypeMapper.base[HashedPassword, String] (
        _.value,
        HashedPassword.apply)
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Table Definitions

These implicit definitions can then be picked up and used to improve the table
definition:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class UsersTable extends Table[(UserId, String, HashedPassword)]("users") {
      def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
      def username = column[String]("username")
      def password = column[HashedPassword]("password")

      def * = id ~ username ~ password

      def usernameIdx = index("idx__users__username", username, unique = true)
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Table Definitions

Secondly, in this situation it'd be more convenient to map the row content to
an actual `User` instance.  We can do this by mapping the default projection,
`*`, using the `User`'s companion object's `apply` and `unapply` methods:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class UsersTable extends Table[User]("users") {
      def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
      def username = column[String]("username")
      def password = column[HashedPassword]("password")

      def * = id ~ username ~ password <> (User, User.unapply _)

      def usernameIdx = index("idx__users__username", username, unique = true)
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Table Definitions

Thirdly, we can arrange for the table definition to be independant of database
driver, using the cake pattern:

## Slick

### Table Definitions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import scala.slick.driver.ExtendedProfile

    trait DatabaseModule {
      val slickProfile: ExtendedProfile
      import slickProfile.simple._
      val database: Database
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Slick's `ExtendedProfile` type provides `simple: SimpleQL` which in-turn
provides us with the ability to define a `Table[R]`, and the type `Database`.

The basic idea is that this module can be declared as a dependency in another
module which describes the user table.

And concrete implementations of the DatabaseModule can be created for the
different database you wish to use.

## Slick

### Table Definitions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait SlickUserTableModule {
      this: DatabaseModule =>        // dependency

      // The Slick database driver is required to define the UsersTable
      import slickProfile.simple._

      val Users = new UsersTable()

      trait UserTypeMappers { /** ellided **/ }

      class UsersTable extends Table[User]("users")
                          with UserTypeMappers {

        def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
        def username = column[String]("username")
        def password = column[HashedPassword]("password")

        def * = id ~ username ~ password <> (User, User.unapply _)

        def usernameIdx = index("idx__users__username", username, unique = true)
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Table Definitions

Finally, concrete implementations of the database module can be written:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait PostgresDatabaseModule extends DatabaseModule {
      import scala.slick.driver.PostgresDriver.simple._
      lazy val slickProfile = scala.slick.driver.PostgresDriver
      lazy val config = ConfigFactory.load()
      lazy val database = Database.forURL(
        config.getString("database.url"),
        driver = config.getString("database.driver"),
        user = config.getString("database.user"),
        password = config.getString("database.password"))
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Table Definitions

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait InMemoryDatabaseModule extends DatabaseModule {
      import scala.slick.driver.H2Driver
      lazy val slickProfile = H2Driver
      import slickProfile.simple._

      lazy val database = Database.forURL(
          s"jdbc:h2:mem:bidding-${randomDBName};DB_CLOSE_DELAY=-1",
          driver = "org.h2.Driver"
      )

      protected def randomDBName = { /** ellided **/ }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Implementing UserService

Re-visit the DataService to provide an implementation backed by a Slick table.

Note that we're not going to define the `UserDAO`, but the `UserService`
itself.

Slick's `Query`s lend themselves to being re-used directly, so rather than
composing together a `UserDAO`s functions, we can compose `Query`s together and
run them all within the same `Session` or even `Transaction`.

## Slick

### Slick UserService

Defining the module...

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait SlickUserServiceModule extends UserServiceModule {
      this: DatabaseModule with SlickUserTableModule =>

      val userService = new Impl()

      class Impl extends UserService { /** ellided **/ }

    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> - `SlickUserTableModule` gives us access to `Users`, the instance of the
>   table object.
> - `DatabaseModule` gives us access to two things:
      1. The `database: Database` which we will use to obtain `Session`s
      2. The `ExtendedProfile` instance which allows us to import the necessary
         functions to construct `Query`s against the database.

## Slick

### Slick UserService

Defining the `Impl`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class Impl extends UserService {
      import slickProfile.simple._

      def create(username: String,
                 plaintextPassword: PlaintextPassword) = { /** ellided */ }

      def authenticate(username: String,
                       plaintextPassword: PlaintextPassword) = {

        val byName = Query(Users).filter(_.username === username)

        database.withSession { implicit s: Session =>
          for {
            user <- byName.firstOption
            matches = checkPassword(plaintextPassword, user.password)
            if matches
          } yield user
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Inserting Rows

So far I've been skipping over this

## Slick

### Inserting Rows

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class UsersTable extends Table[User]("users")
                        with UserTypeMappers {

      def id       = column[UserId]("id", O.PrimaryKey, O.AutoInc)
      def username = column[String]("username")
      def password = column[HashedPassword]("password")

      def * = id ~ username ~ password <> (User, User.unapply _)
      def forInsert = username ~ password

      def usernameIdx = index("idx__users__username", username, unique = true)
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Inserting Rows

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    Users.forInsert returning Users.id insert(username, hashedPassword)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Slick

### Slick UserService

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class Impl extends UserService {
      def create(username: String,
                 plaintextPassword: PlaintextPassword) = {
        try {
          database.withSession { implicit s: Session =>
            val hashedPassword = hashPassword(plaintextPassword)
            val userId =
              Users.forInsert returning Users.id insert (username,
                                                         hashedPassword)
            Success(User(userId, username, hashedPassword))
          }
        } catch {
          // Should really check the SQL error code
          case e: SQLException => Failure(UniqueConstraintException)
          case e: Exception    => Failure(e)
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Spray

## Spray: Providing a REST API

> Spray is an open-source toolkit for building REST/HTTP-based integration
> layers on top of Scala and Akka. Being asynchronous, actor-based, fast,
> lightweight, modular and testable it's a great way to connect your Scala
> applications to the world.

## Spray

Spray has many components and is highly modularized:

> - spray-io : low-level network IO connecting Akka actors to asynchronous
>   Java NIO sockets.
      - Now being migrated into Akka itself.
> - spray-can : Low-level HTTP server and client built on top of spray-io
> - spray-client : High-level HTTP client built on top of spray-can
> - spray-http : Fully immutable, case-class based model of the major HTTP data
>   structures
> - spray-httpx : Higher level logic for working with HTTP messages
      - Marshalling/un-marshalling data
      - (De-)compression
      - Request building
      - Response transformation
> - spray-routing : DSL for describing REST API
> - spray-servlet : Adapter layer for running spray-can server on the Servlet
>   API
> - spray-testkit : DSL for testing routing logic

## Spray

### Overview

> - Routing
> - Marshalling
> - Running on spray-can HTTP server

## Spray

### Routing

Spray-can provides an actor-level interface which allows your application to
respond to incoming `HttpRequest`s by replying with a `HttpResponse` instance:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.http._
    import HttpMethods._

    class MyHttpService extends Actor {
      def receive = {
        case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
          sender ! HttpResponse(entity = "PONG")
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

Alternatively, `spray-routing` provides a DSL for describing the structure of the REST
API using composable elements called `Directive`s.  The above example would be
written as:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.routing._

    class MyHttpService extends HttpServiceActor {
      def receive = runRoute {
        path("ping") {
          get {
            complete("PONG")
          }
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The `runRoute` function constructs a partial function from the structure you've
described in the DSL and applies that to each `HttpRequest`.

## Spray

### Routing

The routing DSL is made available through the `HttpService` trait, which has
one abstract member:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    def actorRefFactory: ActorRefFactory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous example we mixed in `HttpServiceActor` which already defines
the Akka actor context required.

## Spray

### Routes

All structures you build with the routing DSL are subtypes of type `Route`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    type Route = RequestContext => Unit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

Note that the return type is `Unit`, **not** `HttpResponse`!

Instead, a `RequestContext` is *completed*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    ctx.complete(httpResponse)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Which the `ctx` then sends asyncronously to an Akka actor which takes care of
writing the response to the socket.  This allows for chunked responses that are
only available incrementally.

## Spray

### Routing

So, since a `Route` is just an ordinary function, it's simplest form looks
like this:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    ctx => ctx.complete("Response")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternatively:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    _.complete("Response")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or using the `complete` directive:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    complete("Response")
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

A `RequestContext` can also be *rejected*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    ctx => ctx.reject(...)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Which means that this particular route does not want to handle this particular
request.

\pause Or *ignored*:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    ctx => { /** neither completed nor rejected */ }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is generally an error.  If a `RequestContext` is not acted upon (rejected
or completed) then the request will eventually time out, and the user will be
presented with a `500 Internal Server Error` response.

## Spray

### Routing

Routes are composed together to produce complex Routes from simpler building
blocks:

> - Route transformation: Processing is delegated to an inner-route, but in
>   doing so, the incoming request or the outgoing response is modified in some
>   manner.
> - Route filtering: Only letting requests satisfying some predicate pass, all
>   others are rejected.
> - Route chaining: Tries a second `Route` if a first `Route` rejects it.

> - Route transformation and filtering are achieved through the use of
>   `Directive`s.
> - Route chaining is achieved through the `~` operator.

## Spray

### Routing

The routing DSL essentially describes a tree:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val route =
      a {
        b {
          c {
            ... // route 1
          } ~
          d {
            ... // route 2
          } ~
          ... // route 3
        } ~
        e {
          ... // route 4
        }
      }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A request is injected into the root of this tree, and flows down through it
depth-first until some leaf of the tree completes it, *or* it is fully
rejected.

## Spray

### Routing

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val route =
      a {
        b {
          c {
            ... // route 1
          } ~
          d {
            ... // route 2
          } ~
          ... // route 3
        } ~
        e {
          ... // route 4
        }
      }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> - Route 1 will only be reached if directives a, b and c all let the request
>   pass through.

> - Route 2 will run if a and b pass, c rejects and d passes.

> - Route 3 will run if a and b pass, but c and d reject.

## Spray

### Routing

Back to our first example:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.routing._

    class MyHttpService extends HttpServiceActor {
      def receive = runRoute {
        path("ping") {
          get {
            complete("PONG")
          }
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

`Directives` are the building blocks of the routing tree.  They do at least one
of the following:

1. Filter the `RequestContext` according to some logic, i.e. only pass on certain
   requests and reject all others.

2. Extract values from the `RequestContext` and make them available to its inner
   Route as “extractions”.

3. Complete the request.

4. Transform the incoming RequestContext before passing it on to its inner
   Route.

## Spray

### Routing

Filtering directives:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    path("users") {
      get {
        ...
      } ~
      post {
        ...
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`path`, `get` and `post` are all examples of filter directives.

## Spray

### Routing

Exracting values:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    path("users" / LongNumber) { id =>
      get {
        ...
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`path("users" / LongNumber)` extracts a `Long` from the URL and provides it as
the value `id` in the inner route.

## Spray

### Routing

The `path("users")` directive is also an example of a directive which
transforms the incoming `RequestContext` because it extracts the suffix of the
path that was not matched, and passes that in as the path to the inner route.

\pause

Simmilarly, the `HttpResponse` can be altered as it filters back up the matched
path.  Eg.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    path("users" / LongNumber) { id =>
      respondWithHeader(httpHeader) {
        get {
          ...
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

Some useful directives:

> - `get`, `put`, `post`, `delete` match only http requests with those methods.
    - Can be composed together, eg:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
        val getOrPut = get | put
        val route = getOrPut { ... }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> - `path("users")` and `pathPrefix("users")` for matching paths.

> - `IntNumber`, `LongNumber`, `JavaUUID` all extract values from a single
>   segment in a url path.

And user-defined one can also be created.

## Spray

### Routing

We now have the building blocks to describe the API structure for our
UserService.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.routing._

    trait UserRoutes extends HttpService {
      this: UserServiceModule =>

      val userRoutes = pathPrefix("users") {
        path("") {
          post {
            userService.create(???, ???)
            ???
          }
        } ~
        path(PathElement) { username =>
          get {
            userService.find(username)
            ???
          }
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

And also, how we might use the UserService to authenticate users when accessing
a protected resource:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.routing._

    trait PrivateRoutes extends HttpService {
      this: UserServiceModule =>

      /** Required for the UserServiceAuthenticator **/
      import scala.concurrent.{Future, ExecutionContext}
      implicit def executionContext: ExecutionContext = actorRefFactory.dispatcher 

      import spray.routing.authentication._
      val privateRoutes = path("protected") {
        authenticate(BasicAuth(userAthenticator, "realm name")) { user =>
          complete("Secret!")
        }
      }

      private val userAuthenticator = new UserServiceAuthenticator()

      private class UserServiceAuthenticator
              extends UserPassAuthenticator[User] { /** ellided */ }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

Pulling these two route definitions together and serving them using spray-can:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    object Boot extends App {

      // we need an ActorSystem to host our application in
      implicit val system = ActorSystem("on-spray-can")

      // create and start our service actor
      val service = system.actorOf(Props[ServiceActor], "hello-service")

      // start a new HTTP server on port 8080 with our service actor as the handler
      IO(Http) ! Http.Bind(service, "localhost", port = 8080)
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Routing

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    class ServiceActor extends Actor
                          with PrivateRoutes
                          with UserRoutes
                          with SlickUserServiceModule
                          with SlickUserTableModule
                          with InMemoryDatabaseModule {

      // the HttpService trait defines only one abstract member, which
      // connects the services environment to the enclosing actor or test
      def actorRefFactory = context

      // this actor only runs our route, but you could add
      // other things here, like request stream processing,
      // timeout handling or alternative handler registration
      def receive = runRoute(allRoutes)

      lazy val allRoutes = userRoutes ~ privateRoutes
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### Marshalling and Unmarshalling

Conversion of Scala objects to/from `Array[Byte]` for transmission.

- Just going to cover very basic marshalling to/from JSON.

## Spray

### Marshalling

Marshalling in Spray is the conversion of a type `T` to a `HttpEntity`.  It is
performed by a Marshaller:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait Marshaller[T] {
      def apply(value: T, ctx: MarshallingContext): Unit
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\pause

Again, the type the `apply` function is perhaps not expected.  The Spray
documentation cites 3 reasons for this:

1. Marshalling must support content negotiation.  The claim is that this is
   easier if the `Marshaller` drives the process.

2. Using the side-effectful style, the `Marshaller` can delay their action and
   complete the marshalling from another thread (for example, if the result of
   a `Future` arrives).

3. Marshallers can produce content in chunks.

## Spray

### Marshalling

The marshalling in Spray uses type-classes.  That is, to marshall a type `T`,
the compiler must be able to find a `Marshaller[T]` in implicit scope.

## Spray

### Marshalling

There are a whole host of default marshallers available in the `Marshaller`
companion object.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    Array[Byte]
    Array[Char]
    String
    NodeSeq
    Throwable
    spray.http.FormData
    spray.http.HttpEntity

    Option[T]
    Either[A, B]
    Try[T]
    Future[T]
    Stream[T]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Details of the Scala implicit scope resolution mean that because they are
defined on the `Marshaller` companion object, they are always available, but
are still overridable by bringing your own `Marshaller`s into *local* scope).

## Spray

### JSON Marshalling

Spray has it's own JSON library, `spray-json`.  And also `Marshaller`s for each
of the JSON types defined in that library.

## Spray

### JSON Marshalling

`spray-json`'s JSON support is also via type-classes.

Converting an instance of type `T` to a `spray-json` JsValue is achieved using
a `JsonWriter[T]` or a `RootJsonWriter[T]`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait JsonWriter[T] {
      def write(obj: T): JsValue
    }

    trait RootJsonWriter[T] extends JsonWriter[T]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `RootJsonWriter` is capable of writing a JSON array or a JSON object.

## Spray

### JSON Marshalling

Similarly an instance of type `T` *from* a `spray-json` JsValue is achieved using
a `JsonReader[T]` or a `RootJsonReader[T]`:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait JsonReader[T] {
      def write(js: JsValue): T
    }

    trait RootJsonReader[T] extends JsonReader[T]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### JSON Marshalling

Finally, `JsonFormat[T]` combines the two reader and writer traits:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    trait JsonFormat[T] extends JsonReader[T] with JsonWriter[T]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

### JSON Marshalling

To convert our Scala classes to/from JSON we must create an instance of the
above `JsonFormat[T]`.

In the case of case classes, this is very simple:


~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
trait JSONFormats {

  implicit val userIdFormat = jsonFormat1(UserId)

  implicit val userWriter = new RootJsonWriter[User] {
    def write(user: User) = JsObject(
      "username" -> user.name.toJson,
      "id"       -> user.id.toJson)
  }
}
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

- `jsonFormatN` can be used to construct a `JsonFormat[T]` when `T` is a case
  class.
    - There is a variant which accepts alternate field names.

- We haven't constructed a full `JsonFormat[User]` because we cannot construct
  the HashedPassword.  But we have provided the read-side.

## Spray

### JSON Marshalling

> - To tie JSON conversion into the marshalling, spray provides
`spray.httpx.SprayJsonSupport`.

> - It provides a `Marshaller[T]` and
   `Unmarshaller[T]` for every type `T` that an implicit `RootJsonWriter[T]`
    and `RootJsonReader[T]` is available for.

## Spray

### JSON Marshalling

Returning to the route service:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    import spray.httpx.SprayJsonSupport

    trait UserRoutes extends HttpService
                        with JSONFormats
                        with SprayJsonSupport {
      this: UserServiceModule =>

      val userRoutes = pathPrefix("users") {
        path("") {
          post {
            userService.create(???, ???)
            ???
          }
        } ~
        path(PathElement) { username =>
          get {
            rejectEmptyResponse {
              complete(userService.find(username))
            }
          }
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Spray

## JSON Marshalling

By defining a helper case class:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    case class UserCreateData(username: String, password: String)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We can use the same methods to complete the user creation endpoint.

## Spray

### Json Marshalling

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.scala}
    val userRoutes = pathPrefix("users") {
      path("") {
        post {
          entity(as[UserCreateData]) { data =>
            complete {
              userService.create(data.username,
                                 PlaintextPassword(data.password))
                         .toOption
            }
          }
        }
      } ~
      path(PathElement) { username =>
        get {
          rejectEmptyResponse {
            complete(userService.find(username))
          }
        }
      }
    }
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

