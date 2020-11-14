val outwatchVersion = "53a890b" // Get from https://jitpack.io/#outwatch/outwatch builds
Global / onChangedBuildSource := ReloadOnSourceChanges
enablePlugins(ScalaJSBundlerPlugin)
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
scalaVersion := "2.13.3"
scalaJSUseMainModuleInitializer := true
resolvers += "jitpack" at "https://jitpack.io"
libraryDependencies ++= Seq(
  "org.scalactic" %%% "scalactic" % "3.2.2",
  "org.scalatest" %%% "scalatest" % "3.2.2" % "test",
  "org.scala-js"  %%% "scalajs-dom" % "1.1.0",
  "com.github.outwatch.outwatch" %%% "outwatch"       % outwatchVersion,
  "com.github.outwatch.outwatch" %%% "outwatch-monix" % outwatchVersion
)
