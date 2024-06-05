ThisBuild / scalaVersion := "3.4.2"
ThisBuild / scalacOptions ++= Seq("-source", "future", "-deprecation", "-feature")
ThisBuild / fork := false // needed for prompt to work when run from sbt console

lazy val advent2015 = (project in file("2015")).settings(
  name := "advent2015",
).dependsOn(runner, parse, algorithms)

lazy val advent2016 = (project in file("2016")).settings(
  name := "advent2016",
).dependsOn(runner, parse, algorithms)

lazy val advent2019 = (project in file("2019")).settings(
  name := "advent2019",
  libraryDependencies += "co.fs2" %% "fs2-core" % "3.10.2"
).dependsOn(runner, parse, algorithms)

lazy val advent2020 = (project in file("2020")).settings(
  name := "advent2020"
).dependsOn(runner, parse, algorithms)

lazy val advent2022 = (project in file("2022")).settings(
  name := "advent2022",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.4",
    "com.google.ortools" % "ortools-java" % "9.10.4067"
  )
).dependsOn(runner, parse, algorithms)

lazy val advent2023 = (project in file("2023")).settings(
  name := "advent2023",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.5.4"
  )
).dependsOn(runner, parse, algorithms)

lazy val runner = (project in file("runner")).settings(
  name := "runner",
  libraryDependencies ++= Seq(
    "org.http4s" %% "http4s-ember-client" % "0.23.27",
    "org.xerial" % "sqlite-jdbc" % "3.46.0.0",
    "org.tpolecat" %% "doobie-core" % "1.0.0-RC5",
    "com.monovore" %% "decline-effect" % "2.4.1",
    "org.slf4j" % "slf4j-nop" % "2.0.13",
    "org.jsoup" % "jsoup" % "1.17.2",
    "org.scalatest" %% "scalatest" % "3.2.18" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test
  )
).dependsOn(parse)

lazy val algorithms = (project in file("algorithms")).settings(
  name := "algorithms",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.scalatest" %% "scalatest" % "3.2.18" % Test
  )
).dependsOn(parse)

lazy val parse = (project in file("parse")).settings(
  name := "parse",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % "0.14.7",
    "io.circe" %% "circe-generic" % "0.14.7",
    "io.circe" %% "circe-parser" % "0.14.7"
  )
)
