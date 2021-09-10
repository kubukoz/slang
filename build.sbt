val GraalVM11 = "graalvm-ce-java11@20.3.0"

ThisBuild / scalaVersion := "3.0.0"
ThisBuild / githubWorkflowJavaVersions := Seq(GraalVM11)
ThisBuild / githubWorkflowPublishTargetBranches := Nil

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val root = (project in file(".")).settings(
  name := "slang",
  organization := "com.kubukoz",
  scalacOptions ++= Seq("-source", "future"),
  scalacOptions -= "-Xfatal-warnings",
  libraryDependencies ++= Seq(
    "dev.optics" %% "monocle-core" % "3.0.0",
    "org.typelevel" %% "cats-effect" % "3.1.1",
    "co.fs2" %% "fs2-io" % "3.1.2",
    "org.typelevel" %% "cats-parse" % "0.3.4",
    "com.disneystreaming" %% "weaver-cats" % "0.7.6" % Test,
    compilerPlugin("com.kubukoz" % "better-tostring" % "0.3.6" cross CrossVersion.full)
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)
