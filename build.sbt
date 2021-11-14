scalaVersion := "3.1.0"
scalacOptions ++= Seq("-source", "future")
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)
