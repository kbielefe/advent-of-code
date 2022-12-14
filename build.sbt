scalaVersion := "3.2.1"
scalacOptions ++= Seq("-source", "future", "-language:strictEquality", "-deprecation", "-feature")
libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.8.3",
  "com.lihaoyi"   %% "os-lib"    % "0.8.1",
  "org.typelevel" %% "cats-core" % "2.9.0",
  "kbielefe"      %% "puzzle"    % "0.1.0-SNAPSHOT",
  "io.circe" %% "circe-core" % "0.14.1",
  "io.circe" %% "circe-parser" % "0.14.1",
  "org.scalactic" %% "scalactic" % "3.2.14",
  "org.scalatest" %% "scalatest" % "3.2.14" % Test
)
