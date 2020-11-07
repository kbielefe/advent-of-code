scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
coverageEnabled := true
scalaVersion := "2.13.3"
libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.2.2",
                            "org.scalatest" %% "scalatest" % "3.2.2" % "test",
                            "org.scala-graph" %% "graph-core" % "1.13.2",
                            "io.monix" %% "monix" % "3.3.0")
