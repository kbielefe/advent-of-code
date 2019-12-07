scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ypartial-unification", "-language:postfixOps")
coverageEnabled := true
libraryDependencies ++= Seq("org.scalactic" %% "scalactic" % "3.0.5",
                            "org.scalatest" %% "scalatest" % "3.0.5" % "test",
                            "org.scala-graph" %% "graph-core" % "1.12.5",
                            "org.scalaz" %% "scalaz-core" % "7.2.27",
                            "io.monix" %% "monix" % "3.1.0")
