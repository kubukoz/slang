lazy val root = (project in file(".")).settings(
  name := "slang",
  organization := "com.kubukoz",
  scalaVersion := "3.0.0-RC1",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
    "co.fs2" %% "fs2-io" % "3.0.0-M9",
    "org.typelevel" %% "cats-parse" % "0.3.1"
  )
)
