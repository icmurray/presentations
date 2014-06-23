import sbt._
import Keys._

object ScalaFPIntro extends Build {

  lazy val globalSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion in ThisBuild := "2.10.4",
    version in ThisBuild := "0.1-SNAPSHOT",
    scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature"),

    resolvers ++= Seq(
      "TypeSafe Releases" at "http://repo.typesafe.com/typesafe/releases"
    ),

    // Standardise some common dependencies.
    libraryDependencies ++= Seq(
    )

  )

  lazy val root       = Project(id        = "scala-fp-intro",
                                base      = file("."),
                                aggregate = Seq(javaLike, scalaLike, scalazLike))

  lazy val common     = Project(id        = "common",
                                base      = file("common"),
                                settings  = globalSettings)

  lazy val javaLike   = Project(id        = "java-like",
                                base      = file("javaLike"),
                                settings  = globalSettings)
                          .dependsOn(common)

  lazy val scalaLike  = Project(id        = "scala-like",
                                base      = file("scalaLike"),
                                settings  = globalSettings)
                          .dependsOn(common)

  lazy val scalazLike = Project(id        = "scalaz-like",
                               base      = file("scalazLike"),
                               settings  = globalSettings ++ Seq(
                                 libraryDependencies ++= Seq(
                                   "org.scalaz"      %% "scalaz-core"         % "7.0.6",
                                   "org.typelevel"   %% "scalaz-contrib-210"  % "0.1.5"
                                 )
                               ))
                          .dependsOn(common)
}
