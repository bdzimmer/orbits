// Copyright (c) 2017 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import org.scalatest.FunSuite

import java.io.File
import javax.imageio.ImageIO

import bdzimmer.util.TempDirectory
import bdzimmer.util.StringUtils._


class IntegrationSuite extends FunSuite with TempDirectory {

  val compass = ConstAccelCraft("Test", 30000.0, 0.2)
  val startDate = CalendarDateTime(2016, 7, 27, 0)
  val endDate   = CalendarDateTime(2016, 7, 31, 0)

  val startPlanet = MeeusPlanets.Mars.planet
  val startPlanetName = "Mars"
  val endPlanet = MeeusPlanets.Earth.planet
  val endPlanetName = "Earth"


  test("draw flight") {

    val im = RenderFlight.drawFlight(
        compass, "test faction",
        startPlanetName, endPlanetName,
        startPlanet, endPlanet, startDate, endDate)

    val outputImage = new java.io.File(tempDirname / "output.png");
    ImageIO.write(im, "png", outputImage)

    assert(outputImage.exists)

    // for visualization with other programs

    val outputFilename = tempDirname / "test.csv"
    val outputFile = new File(outputFilename)
    val pw = new java.io.PrintWriter(outputFile)

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    for (tick <- startDateJulian until endDateJulian by (1.0 / 24)) {

      val earthState = Orbits.planetState(startPlanet, tick)
      pw.println(Orbits.stateString(startPlanetName, tick, earthState))

      val marsState = Orbits.planetState(endPlanet, tick)
      pw.println(Orbits.stateString(endPlanetName, tick, marsState))

    }

    pw.close()

    assert(outputFile.exists)

  }


  test("animate flight") {

    val outputDir = new java.io.File(tempDirname / "testflight")
    outputDir.mkdirs()

    RenderFlight.animateFlight(
        compass,
        startPlanetName, endPlanetName,
        startPlanet, endPlanet, startDate, endDate,
        outputDir.getAbsolutePath)

    assert(outputDir.list.length > 0)

    val outputFile = new java.io.File(tempDirname / "testflight.mp4")
    RenderFlight.imagesToVideo(outputDir.getAbsolutePath, outputFile.getAbsolutePath, 800, 600, 30)

    assert(outputFile.exists)

  }


}