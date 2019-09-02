// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Functionality for rendering movies.

package bdzimmer.orbits

import java.awt.Color
import java.awt.image.BufferedImage
import java.util

// import javax.imageio.ImageIO
import org.apache.commons.imaging.{ImageFormats, Imaging}
import bdzimmer.util.StringUtils._


case class AnimationSettings(
  width: Int,
  height: Int,
  camPos: Vec3,
  zViewPos: Double,
  fps: Int,
  interval: Double,
  damping: Double
)


object Animation {

  // TODO: lots of extra stuff needs to go here
  def animateFlights(
      flights: List[FlightParams],   // all flights in epoch
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      factions: Map[String, Color],
      showSettings: ShowSettings,
      animationSettings: AnimationSettings,
      outputDirname: String): Unit = {

    def getActiveFlights(tick: Double): List[FlightParams] = {
      flights.filter(x => tick >= x.startDate.julian && tick < x.endDate.julian)
    }

    val im = new BufferedImage(
      animationSettings.width, animationSettings.height, BufferedImage.TYPE_INT_ARGB)

    // lot of duplicate code between this and Editor.draw, but that's ok for now
    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    val planets = showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList

    // find initial camera and viewer position
    // for now, these don't change
    val initCamPos = animationSettings.camPos
    val initViewPos = Vec3(0, 0, animationSettings.zViewPos)

    // calculate initial state: mean of all active flights
    // this doesn't include damping
    val initActiveFlights = getActiveFlights(startDateJulian)

    var prevState = if (initActiveFlights.nonEmpty) {
      // average flight position at start date
      Vec3.mul(
        sumStates(initActiveFlights, startDateJulian),
        1.0 / initActiveFlights.length)
    } else {
      // anticipate average position of first flight(s)
      val firstFlightDateJulian = flights.map(_.startDate.julian).min
      println(firstFlightDateJulian)
      // TODO: may have to advance this by a small fraction of a day or something
      val firstActiveFlights = getActiveFlights(firstFlightDateJulian)
      println(firstActiveFlights.length)
      Vec3.mul(
        sumStates(firstActiveFlights, firstFlightDateJulian),
        1.0 / firstActiveFlights.length)
      // Vec3(0.0, 0.0, 0.0)
    }


    val ticks = (startDateJulian to endDateJulian by animationSettings.interval).toList.toIndexedSeq

    ticks.zipWithIndex.foreach({case (tick, idx) => {

      val initTimeStart = System.currentTimeMillis

      val activeFlights = getActiveFlights(tick)

      val activeFlightsTime = System.currentTimeMillis - initTimeStart // interested in how much time this is

      // TODO: may not want to highlight a particular flight
      // in that case fpOption should be None
      val fpOption = activeFlights.reduceOption((x, y) => if (x.startDate.julian < y.startDate.julian) x else y)

      val dampedComponent = Vec3.mul(prevState, Editor.Damping)
      val sumComponent = sumStates(activeFlights, tick)

      // weighted average of active flights and previous position
      val curState = Vec3.mul(
        Vec3.add(dampedComponent, sumComponent),
        1.0 / (activeFlights.length + Editor.Damping))

      // update previous position
      prevState = curState
      val camRot = Editor.pointCamera(curState, initCamPos)
      val camTrans = View.cameraTransform(camRot, initCamPos)

      val initTime = System.currentTimeMillis - initTimeStart

      // ~~~~ ~~~~ ~~~~

      val drawTimeStart = System.currentTimeMillis

      Draw.redraw(
        fpOption,
        tick,
        planets,
        flights,
        factions,
        showSettings.asteroidBelt,
        showSettings.lagrangePoints,
        showSettings.flightStatus,
        camTrans,
        initViewPos,
        im
      )

      val drawTime = System.currentTimeMillis - drawTimeStart

      // ~~~~ ~~~~ ~~~~

      val writeTimeStart = System.currentTimeMillis

      val outputFilename = new java.io.File(outputDirname / f"$idx%05d.png")

      // ImageIO.write(im, "png", outputFilename)
      Imaging.writeImage(im, outputFilename, ImageFormats.PNG, new util.HashMap[String, Object]())

      val writeTime = System.currentTimeMillis - writeTimeStart

      // ~~~~ ~~~~ ~~~~
      // show "profiling"

      println(
        // Conversions.julianToCalendarDate(tick),
        // activeFlights.length,
        outputFilename + " "  +
        idx + " / " + ticks.length + " " +
        Conversions.julianToCalendarDate(tick).dateTimeString + " " +
        "(" + initTime + " [" + activeFlightsTime + "] " + drawTime + " " + writeTime + ")")

    }})

    RenderFlight.imagesToVideo(
      outputDirname,
      outputDirname / "animation.mp4",
      animationSettings.width,
      animationSettings.height,
      animationSettings.fps)

  }


  // sum of flight states; used for damping
  def sumStates(flights: List[FlightParams], date: Double): Vec3 = {
    flights.map(fp => {
      val (flightFn, _) = Editor.paramsToFun(fp)
      flightFn(date)
    }).foldLeft(Vec3(0.0, 0.0, 0.0))(Vec3.add)
  }


}
