// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// orbits project build.sbt file

val whichJvmSettings = sys.props.getOrElse("jvm", default = "7")
val jvmSettings = whichJvmSettings match {
  case "6" => JvmSettings("1.6", "1.6", "1.6")
  case _   => JvmSettings("1.7", "1.7", "1.7")
}


lazy val root = (project in file("."))
  .settings(
    name         := "orbits",
    version      := "2016.07.03",
    organization := "bdzimmer",
    scalaVersion := "2.10.6",
    
    mainClass in (Compile, run) := Some("bdzimmer.orbits.Orbits"),

    javacOptions  ++= Seq("-source", jvmSettings.javacSource, "-target", jvmSettings.javacTarget),
    scalacOptions ++= Seq(s"-target:jvm-${jvmSettings.scalacTarget}"),

    libraryDependencies ++= Seq(
      "commons-io"         % "commons-io"                % "2.4",
      "org.scalatest"     %% "scalatest"                 % "2.2.4" % "it,test"
    ),
    
    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))
    
   )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)

EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE17)

EclipseKeys.withBundledScalaContainers := false
