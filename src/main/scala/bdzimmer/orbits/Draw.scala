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

      im: BufferedImage): Unit = {

    val view = new Viewer(camTrans, viewPos, DisplaySettings)

    // helper - draw velocity arrow
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

    val planetMotions = planets.map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2, curDateJulian))
    })

    // find other flights that are active at the same time as the current one
    val activeFlights = flights.filter(x =>
      // !x.equals(fp) &&
      x.startDate.julian <= curDateJulian && x.endDate.julian >= curDateJulian)

    // clear the image
    val gr = im.getGraphics
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, im.getWidth, im.getHeight)

    // ~~~~

    // draw the grid and the sun
    view.drawGrid(im, GridLim, new Color(0, 0, 128))
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw the orbits of planets and their positions
    // the sequence of orbital states for each planet should start from same time
    // as the final state of the flight - this is the time that we are drawing the
    // flight at
    planetMotions.foreach(x => RenderFlight.drawOrbit(im, x._2, view))
    planetMotions.foreach(x => view.drawPosition(im, x._2.last.position, x._1, "", Color.GRAY))

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

    // draw the asteroid belt
    if (asteroidBelt) {
      view.drawRing(im, BeltR0, BeltR1, new Color(64, 64, 64, 128))
    }

    // draw other flights in the background
    // TODO: optional ship arrows and names?
    // otherFlights.zip(otherFlightsColors).toList.foreach(x => view.drawMotion(im, x._1, x._2))

    activeFlights.foreach(flight => {
        val (afFn, afTicks) = Editor.paramsToFun(flight)
        val positions = afTicks.filter(x => x <= curDateJulian).map(tick => afFn(tick))
        val factionColor = factions.getOrElse(flight.faction, Color.GRAY)

        // draw motion and velocity arrow
        view.drawMotion(im, positions, factionColor)
        val vel = drawVelocityArrow(afFn, factionColor)

        // draw status
        // val pos2d = View.perspective(positions.last, camTrans, viewPos)    // wrong
        // TODO: make this optional in ShowSettings
        val pos2d = View.perspective(afFn(curDateJulian), camTrans, viewPos)
        val (x, y) = view.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)
        RenderFlight.drawFlightStatus(
          x, y,
          im,
          flight.ship,
          flight.faction,
          Color.green,
          Conversions.julianToCalendarDate(curDateJulian),
          Vec3.length(Vec3.sub(positions.last, positions.head)),
          vel,
          DisplaySettings
        )

    })


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