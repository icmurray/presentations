import sbt._
import Keys._

import play.Project._

object ScalaTalkExample extends Build {

  lazy val globalSettings = Defaults.defaultSettings ++ Seq(
    scalaVersion in ThisBuild := "2.10.3",
    libraryDependencies ++= Seq(
       "org.scalatest"  %% "scalatest"    % "1.9.2"   % "test"),
    resolvers ++= Seq(
      "Spray Repo" at "http://repo.spray.io/"
    )
  )

  lazy val root      = Project(id        = "hello",
                               base      = file("."),
                               aggregate = Seq(core, cli, api))

  lazy val core      = Project(id        = "hello-core",
                               base      = file("core"),
                               settings  = globalSettings ++ Seq(
                                 libraryDependencies ++= Seq(
                                   "com.typesafe"          % "config"              % "0.4.0",
                                   "com.typesafe.slick"    %% "slick"              % "1.0.1",
                                   "com.h2database"        % "h2"                  % "1.3.166",
                                   "joda-time"             %  "joda-time"          % "2.2",
                                   "org.joda"              %  "joda-convert"       % "1.3.1",
                                   "org.mindrot"           % "jbcrypt"             % "0.3m",
                                   "com.typesafe.akka"     %%  "akka-actor"        % "2.2.0-RC1")
                               ))
                                
  lazy val api       = Project(id        = "hello-api",
                               base      = file("api"),
                               settings  = globalSettings ++ Seq(
                                 libraryDependencies ++= {
                                   val sprayVersion = "1.2-M8"
                                   Seq(
                                     "io.spray"            %   "spray-can"     % sprayVersion,
                                     "io.spray"            %   "spray-routing" % sprayVersion,
                                     "io.spray"            %   "spray-testkit" % sprayVersion,
                                     "io.spray"            %%  "spray-json"    % "1.2.5")
                                 }
                               )) dependsOn(core)

  lazy val cli       = Project(id        = "hello-cli",
                               base      = file("cli")) dependsOn(core)

  lazy val frontend  = play.Project("hello-frontend",
                               path = file("frontend")
                              ) dependsOn(core) settings(
                                templatesImport ++= Seq(
                                 "uk.co.sprily.scalaTalk._")
                              )
}
