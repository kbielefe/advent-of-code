lazy val commonSettings = Seq(
  scalaVersion := "3.1.0",
  scalacOptions ++= Seq("-source", "future", "-language:strictEquality")
)

lazy val root = (project in file(".")).settings(
  name := "advent of code",
  publish / skip := true
).aggregate(aoc, puzzleparse, visualization)

lazy val aoc = (project in file("aoc")).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "com.softwaremill.sttp.client3" %% "core" % "3.3.18",
    "com.lihaoyi" %% "os-lib" % "0.8.0",
    "org.typelevel" %% "cats-core" % "2.7.0"
  )
).dependsOn(puzzleparse)

lazy val puzzleparse = (project in file("puzzleparse")).settings(
  name := "puzzleparse",
  commonSettings,
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
    "org.scalactic" %% "scalactic" % "3.2.10",
    "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )
)

lazy val visualization = (project in file("visualization")).settings(
  name := "visualization",
  commonSettings,
  libraryDependencies ++= Seq(
    "io.monix" %% "monix" % "3.4.0"
  )
)
