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

