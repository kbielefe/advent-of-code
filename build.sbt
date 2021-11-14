scalaVersion := "3.1.0"
scalacOptions ++= Seq("-source", "future")
libraryDependencies ++= Seq(
  "com.softwaremill.sttp.client3" %% "core" % "3.3.16",
  "com.lihaoyi" %% "os-lib" % "0.7.8",
  "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)
