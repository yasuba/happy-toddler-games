ThisBuild / scalaVersion := "3.3.7"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "toddler-calm-app",
    version := "0.1.0",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,

    scalaJSUseMainModuleInitializer := true,
    Compile / mainClass := Some("toddler.Main"),

    libraryDependencies ++= Seq(
      "io.indigoengine" %%% "indigo" % "0.16.0"
    )
  )
  
