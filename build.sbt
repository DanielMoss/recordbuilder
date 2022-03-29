ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "recordbuilder",
    libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.8",
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
  )
