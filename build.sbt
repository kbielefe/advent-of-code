import org.scalajs.linker.interface.ModuleSplitStyle

ThisBuild / scalaVersion := "3.6.2"
ThisBuild / scalacOptions ++= Seq("-source", "future", "-deprecation", "-feature")
ThisBuild / fork := false // needed for prompt to work when run from sbt console

lazy val advent2023 = (project in file("2023")).settings(
  name := "advent2023"
).dependsOn(runner, algorithms)

lazy val advent2024 = (project in file("2024")).settings(
  name := "advent2024"
).dependsOn(runner, algorithms)

lazy val runner = (project in file("runner")).settings(
  name := "runner",
  libraryDependencies ++= Seq(
    "org.http4s" %% "http4s-ember-client" % "0.23.30",
    "org.xerial" % "sqlite-jdbc" % "3.47.1.0",
    "org.tpolecat" %% "doobie-core" % "1.0.0-RC6",
    "com.monovore" %% "decline-effect" % "2.4.1",
    "org.slf4j" % "slf4j-nop" % "2.0.16",
    "org.jsoup" % "jsoup" % "1.18.3",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test
  )
).dependsOn(parse, visualizations.jvm)

lazy val algorithms = (project in file("algorithms")).settings(
  name := "algorithms",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.12.0",
    "co.fs2" %% "fs2-core" % "3.11.0",
    "org.typelevel" %% "spire" % "0.18.0",
    "org.scalanlp" %% "breeze" % "2.1.0",
    "org.typelevel" %% "cats-collections-core" % "0.9.9",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test
  )
).dependsOn(parse)

lazy val parse = (project in file("parse")).settings(
  name := "parse",
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % "0.14.10",
    "io.circe" %% "circe-generic" % "0.14.10",
    "io.circe" %% "circe-parser" % "0.14.10"
  )
)

lazy val visualizations = crossProject(JSPlatform, JVMPlatform)
  .in(file("visualizations"))
  .settings(
    name := "visualizations",
  ).jvmSettings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % "0.23.30",
      "org.http4s" %% "http4s-dsl"          % "0.23.30",
      "org.http4s" %% "http4s-circe"        % "0.23.30"
    )
  ).jsSettings(
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(
          ModuleSplitStyle.SmallModulesFor(List("visualizations")))
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.8.0",
      "org.http4s" %%% "http4s-dom" % "0.2.11",
      "org.http4s" %%% "http4s-client" % "0.23.30",
      "io.circe" %%% "circe-core" % "0.14.10",
      "io.circe" %%% "circe-generic" % "0.14.10",
      "io.circe" %%% "circe-parser" % "0.14.10",
      "org.http4s" %%% "http4s-circe" % "0.23.30",
    )
  ).jvmConfigure(_.dependsOn(algorithms))

lazy val leetcode = (project in file("leetcode")).settings(
  name := "leetcode",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.12.0",
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  )
)
