lazy val root = (project in file(".")).settings(
  name := "slang",
  organization := "com.kubukoz",
  scalaVersion := "3.0.0-M3",
  scalacOptions --= Seq("-Xfatal-warnings"),
  libraryDependencies ++= Seq(
    "com.github.julien-truffaut" %% "monocle-core" % "3.0.0-M2",
    "org.typelevel" %% "cats-effect" % "3.0.0-RC2",
    "co.fs2" %% "fs2-io" % "3.0.0-M9",
    "org.typelevel" %% "cats-parse" % "0.3.1",
    "io.circe" %% "circe-core" % "0.14.0-M3"
  )
)
