enablePlugins(ScalaJSPlugin)
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
scalaVersion := "2.13.3"
scalaJSUseMainModuleInitializer := true
libraryDependencies ++= Seq(
  "org.scalactic" %%% "scalactic" % "3.2.2",
  "org.scalatest" %%% "scalatest" % "3.2.2" % "test",
  "io.monix"      %%% "monix" % "3.3.0"
)
