val scala3Version = "3.8.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "FPlab2",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2"
  )
