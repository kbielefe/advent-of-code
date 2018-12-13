scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scala-graph" %% "graph-core" % "1.12.5"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27"
libraryDependencies += "io.monix" %% "monix" % "3.0.0-RC2"
coverageEnabled := true
