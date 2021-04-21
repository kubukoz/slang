val GraalVM11 = "graalvm-ce-java11@20.3.0"

ThisBuild / scalaVersion := "3.0.0-RC2"
ThisBuild / githubWorkflowJavaVersions := Seq(GraalVM11)
ThisBuild / githubWorkflowPublishTargetBranches := Nil

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file(".")).settings(
  name := "slang",
  organization := "com.kubukoz",
  scalacOptions --= Seq("-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M4",
    "org.typelevel" %% "cats-effect" % "3.1.0",
    "co.fs2" %% "fs2-io" % "3.0.1",
    "org.typelevel" %% "cats-parse" % "0.3.2",
    "com.disneystreaming" %% "weaver-cats" % "0.7.1" % Test,
    compilerPlugin("com.kubukoz" %% "better-tostring" % "0.2.10")
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)
