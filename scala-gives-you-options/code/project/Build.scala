import sbt._
import Keys._

object ScalaGivesYouOptions extends Build {

  lazy val globalSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion in ThisBuild := "2.10.4",
    version in ThisBuild := "0.1-SNAPSHOT",
    scalacOptions in ThisBuild ++= Seq("-deprecation", "-unchecked", "-feature"),

    resolvers ++= Seq(
      "TypeSafe Releases" at "http://repo.typesafe.com/typesafe/releases"
    ),

    // Standardise some common dependencies.
    libraryDependencies ++= Seq(
      "org.scalaz"      %% "scalaz-core"         % "7.0.6",
      "org.typelevel"   %% "scalaz-contrib-210"  % "0.1.5"
    )

  )

  lazy val root       = Project(id        = "scala-gives-you-options",
                                base      = file("."),
                                settings  = globalSettings)
}
