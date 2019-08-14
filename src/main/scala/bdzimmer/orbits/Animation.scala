// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Functionality for rendering movies.

package bdzimmer.orbits

import java.awt.Color
import java.awt.image.BufferedImage

import javax.imageio.ImageIO

import bdzimmer.util.StringUtils._


case class AnimationSettings(
  camPos: Vec3,
  zViewPos: Double,
  fps: Int,
  interval: Double,
  damping: Double
  // TODO: other options such as canvas size
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
      flights.filter(x => tick > x.startDate.julian && tick < x.endDate.julian)
    }

    // TODO: make these parameters
    val imWidth = 1280 / 2
    val imHeight = 720 / 2

    val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)

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

    var prevState = if (initActiveFlights.length > 0.0) {
      Vec3.mul(
        sumStates(initActiveFlights, startDateJulian),
        1.0 / initActiveFlights.length)
    } else {
      Vec3(0.0, 0.0, 0.0)
    }

    val ticks = (startDateJulian to endDateJulian by animationSettings.interval).toList.toIndexedSeq

    ticks.zipWithIndex.foreach({case (tick, idx) => {

      val activeFlights = getActiveFlights(tick)

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

      val outputFilename = new java.io.File(outputDirname / f"$idx%05d.png")
      ImageIO.write(im, "png", outputFilename)
      println(Conversions.julianToCalendarDate(tick), activeFlights.length, idx + " / " + ticks.length + " " + outputFilename)

    }})

    RenderFlight.imagesToVideo(
      outputDirname,
      outputDirname / "animation.mp4",
      imWidth, imHeight, animationSettings.fps)


  }


  // sum of flight states; used for damping
  def sumStates(flights: List[FlightParams], date: Double): Vec3 = {
    flights.map(fp => {
      val (flightFn, _) = Editor.paramsToFun(fp)
      flightFn(date)
    }).foldLeft(Vec3(0.0, 0.0, 0.0))(Vec3.add)
  }


}
