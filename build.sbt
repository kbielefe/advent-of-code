ThisBuild / scalaVersion := "3.3.1"
ThisBuild / scalacOptions ++= Seq("-source", "future", "-language:strictEquality", "-deprecation", "-feature")

lazy val advent2023 = (project in file("2023")).settings(
  name := "advent2023"
).dependsOn(runner, algorithms, parse)

lazy val runner = (project in file("runner")).settings(
  name := "runner",
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.client3" %% "core" % "3.9.1",
    "com.lihaoyi"   %% "os-lib" % "0.9.2"
  )
)

lazy val algorithms = (project in file("algorithms")).settings(
  name := "algorithms"
)

lazy val parse = (project in file("parse")).settings(
  name := "parse"
)
