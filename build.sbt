val zioVersion = "2.0.21"
val zioHttpVersion = "3.0.0-RC3"
val scalaCsvVersion = "1.3.10"

val scala3Version = "3.3.1"

ThisBuild / organization := "fr.efrei"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3Version


lazy val root = (project in file("."))
  .settings(
    name := "Scala-Zio-Project",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-http" % zioHttpVersion,
      "com.github.tototoshi" %% "scala-csv" % scalaCsvVersion,
      "com.h2database" % "h2" % "2.1.214",
      "dev.zio" %% "zio-test" % zioVersion % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
