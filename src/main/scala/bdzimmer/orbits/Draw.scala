// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Functionality for drawing multiple flights, planets, and flight paths onto an image.

package bdzimmer.orbits

import java.awt.Color
import java.awt.image.BufferedImage

import scala.collection.immutable.Seq


object Draw {

  val GridLim = 50  // radius of solar system is about 50 AU

  // draw asteroid belt - main belt lies between 2.06 and 3.27 AU
  val BeltR0 = 2.06
  val BeltR1 = 3.27

  val EpsVel = 1.0e-4  // for velocity calculation

  val DisplaySettings = Viewer.ViewerSettingsArtsy


  def redraw(

      fpOption: Option[FlightParams],       // flight to highlight with status
      curDateJulian: Double,

      planets: Seq[(String, OrbitalElementsEstimator)],
      flights: Seq[FlightParams],
      factions: Map[String, Color],

      asteroidBelt: Boolean,
      lagrangePoints: Boolean,
      statusOption: Int,

      camTrans: Mat44,
      viewPos: Vec3,

      im: BufferedImage
    ): Unit = {

    val planetMotions = planets.map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2, curDateJulian))
    })

    // find other flights that are active at the same time as the current one
    val activeFlights = flights.filter(x =>
      // !x.equals(fp) &&
      x.startDate.julian <= curDateJulian && x.endDate.julian >= curDateJulian)
    val activeFlightFns = activeFlights.map(af => Editor.paramsToFun(af))
    val otherFlights = activeFlightFns.map({
      case (afFn, afTicks) => afTicks.filter(x => x <= curDateJulian).map(tick => afFn(tick))
    })

    val view = new Viewer(camTrans, viewPos, DisplaySettings)

    // clear the image
    val gr = im.getGraphics
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, im.getWidth, im.getHeight)

    val otherFlightsColors = activeFlights.map(x => factions.getOrElse(x.faction, Color.GRAY))

    RenderFlight.drawStateAtTime(
      view, im, planetMotions, otherFlights.zip(otherFlightsColors).toList, GridLim)

    if (asteroidBelt) {
      view.drawRing(im, BeltR0, BeltR1, new Color(64, 64, 64, 128))
    }

    // draw L3, L4 and L5 points of visible planets
    if (lagrangePoints) {
      planets.foreach(p => {
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L3Estimator(p._2), curDateJulian).position,
          "L3", "", Color.GRAY, fill = false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L4Estimator(p._2), curDateJulian).position,
          "L4", "", Color.GRAY, fill = false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L5Estimator(p._2), curDateJulian).position,
          "L5", "", Color.GRAY, fill = false)
      })
    }

    // draw velocity direction arrows for all active flights

    def drawVelocityArrow(flightFn: FlightFn, color: Color): Double = {
      if (curDateJulian - EpsVel > flightFn.startTime) {
        val curState = flightFn(curDateJulian)
        val curVelVec = Vec3.mul(
          Vec3.sub(curState, flightFn(curDateJulian - EpsVel)),
          1.0 / EpsVel)
        val curVel = Vec3.length(curVelVec)
        if (curVel > ConstVelFlightFn.VelMin) {
          view.drawArrow(im, OrbitalState(curState, curVelVec), color)
        }
        curVel
      } else {
        0.0
      }
    }

    activeFlightFns.zip(otherFlightsColors).foreach({case (x, y) => drawVelocityArrow(
      x._1, y)})


    fpOption.foreach(fp => {

      val (flightFn, ticks) = Editor.paramsToFun(fp)

      if (ticks.length < 1) {
        return
      }

      // to plot how the origin and desination change curing the flight
      val origStates = ticks.map(tick => Orbits.planetState(fp.orig, tick))
      val destStates = ticks.map(tick => Orbits.planetState(fp.dest, tick))
      val ticksSubset = ticks.takeWhile(x => x < curDateJulian)
      val flightStates = ticksSubset.map(tick => flightFn(tick))
      val flightColor = factions.getOrElse(fp.faction, Color.GREEN)

      RenderFlight.drawHighlightedFlightAtTime(
        view, im, fp.origName, fp.destName, fp.startDate.dateString, fp.endDate.dateString,
        origStates, destStates, flightStates, flightColor)

      // prepare summary or description

      val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

      // average velocity
      val vel = distance / (fp.endDate.julian - fp.startDate.julian)

      val curVel = drawVelocityArrow(
        flightFn, flightColor)

      // val statusOption = flightStatusRadioButtons.indexWhere(_.isSelected)

      if (statusOption == 0) {
        // draw flight status with current datetime, distance, and velocity
        RenderFlight.drawFlightStatus(
          im,
          fp.ship,
          fp.faction,
          Conversions.julianToCalendarDate(curDateJulian),
          Vec3.length(Vec3.sub(flightStates.last, flightStates.head)),
          curVel,
          DisplaySettings)
      } else if (statusOption == 1) {
        // draw flight summary
        fp.ship match {
          case x: ConstAccelCraft => {
            val accel = flightFn match {
              case y: ConstAccelFlightFn => y.accel
              case _ => 0.0
            }
            RenderFlight.drawFlightSummary(
              im, x, fp.faction, distance, vel, accel,
              fp.origName, fp.destName, fp.startDate, fp.endDate,
              DisplaySettings)
          }
          case x: ConstVelCraft => RenderFlight.drawFlightSummary(
            im, x, fp.faction, distance, vel,
            fp.origName, fp.destName, fp.startDate, fp.endDate,
            DisplaySettings)
        }

      }

    })

  }

}