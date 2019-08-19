// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// orbits project build.sbt file

val jvmSettings = JvmSettings("1.8", "1.8", "1.8")


lazy val root = (project in file("."))
  .settings(
    name         := "orbits",
    version      := "2016.07.03",
    organization := "bdzimmer",
    scalaVersion := "2.10.6",

    mainClass in (Compile, run) := Some("bdzimmer.orbits.Orbits"),

    javacOptions  ++= Seq("-source", jvmSettings.javacSource, "-target", jvmSettings.javacTarget),
    scalacOptions ++= Seq(s"-target:jvm-1.7"),

    libraryDependencies ++= Seq(
      "commons-io"         % "commons-io"      % "2.4",
      "org.apache.commons" % "commons-imaging" % "1.0-alpha1",
      "org.scalatest"     %% "scalatest"       % "2.2.4" % "it,test"
    ),

    unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_)),
    unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

   )
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)
  .dependsOn(utilscala)

lazy val utilscala = RootProject(file("../util-scala"))

EclipseKeys.projectFlavor := EclipseProjectFlavor.Scala

EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18)

EclipseKeys.withBundledScalaContainers := false
