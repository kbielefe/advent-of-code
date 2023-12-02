ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq("-source", "future", "-language:strictEquality", "-deprecation", "-feature")
ThisBuild / fork := false // needed for prompt to work when run from sbt console

lazy val advent2015 = (project in file("2015")).settings(
  name := "advent2015",
).dependsOn(runner, parse, algorithms)

lazy val advent2016 = (project in file("2016")).settings(
  name := "advent2016",
).dependsOn(runner, parse, algorithms)

lazy val advent2019 = (project in file("2019")).settings(
  name := "advent2019",
  libraryDependencies += "co.fs2" %% "fs2-core" % "3.9.3"
).dependsOn(runner, parse, algorithms)

lazy val advent2022 = (project in file("2022")).settings(
  name := "advent2022",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.2",
    "com.google.ortools" % "ortools-java" % "9.8.3296"
  )
).dependsOn(runner, parse, algorithms)

lazy val advent2023 = (project in file("2023")).settings(
  name := "advent2023",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.2"
  )
).dependsOn(runner, parse, algorithms)

lazy val runner = (project in file("runner")).settings(
  name := "runner",
  libraryDependencies ++= Seq(
    "org.http4s" %% "http4s-ember-client" % "0.23.24",
    "org.xerial" % "sqlite-jdbc" % "3.44.0.0",
    "org.tpolecat" %% "doobie-core" % "1.0.0-RC4",
    "com.monovore" %% "decline-effect" % "2.4.1",
    "org.slf4j" % "slf4j-nop" % "2.0.9",
    "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test
  )
).dependsOn(parse)

lazy val algorithms = (project in file("algorithms")).settings(
  name := "algorithms",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.17" % Test
  )
).dependsOn(parse)

lazy val parse = (project in file("parse")).settings(
  name := "parse",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % "0.14.1",
    "io.circe" %% "circe-generic" % "0.14.1",
    "io.circe" %% "circe-parser" % "0.14.1"
  )
)
