// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// orbits project build.scala file

import sbt._
import Keys._

case class JvmSettings(javacSource: String, javacTarget: String, scalacTarget: String)