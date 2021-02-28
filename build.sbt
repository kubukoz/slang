
ThisBuild / scalaVersion := "3.0.0-M3"

val GraalVM11 = "graalvm-ce-java11@20.3.0"

ThisBuild / githubWorkflowJavaVersions := Seq(GraalVM11)

lazy val root = (project in file(".")).settings(
  name := "slang",
  organization := "com.kubukoz",
  scalaVersion := "3.0.0-M3",
  scalacOptions --= Seq("-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    // M3 supports only 3.0.0-RC1
    // we're blocked on weaver for RC1 though :(
    "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M2",
    "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
    "co.fs2" %% "fs2-io" % "3.0.0-M9",
    "org.typelevel" %% "cats-parse" % "0.3.1",
    "com.disneystreaming" %% "weaver-cats" % "0.7.0-M6" % Test
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect")
)
