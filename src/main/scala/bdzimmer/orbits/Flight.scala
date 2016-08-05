// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Rough interplanetary flight planning.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage


object Flight {

  def roughFlightGivenTime(
      startPos: Vec3, endPos: Vec3, flightTime: Double, res: Double): (Seq[Vec3], Double) = {

    val path = Vec3.sub(endPos, startPos)
    val pathLength = Vec3.length(path)
    val dir = Vec3(path.x / pathLength, path.y / pathLength, path.z / pathLength)

    val accel = 4 * pathLength / (flightTime * flightTime)
    (roughFlight(dir, accel, flightTime, res).map(x => Vec3.add(startPos, x)), accel)

  }


  def roughFlight(dir: Vec3, accel: Double, flightTime: Double, res: Double): Seq[Vec3] = {

    val ticks = (0.0 to flightTime by res).toIndexedSeq
    val halfFlightTime = flightTime / 2

    val pos1d = ticks.map(t => {
      if (t < halfFlightTime) {
        0.5 * accel * t * t
      } else {
        -0.5 * accel * t * t + accel * flightTime * t - 0.25 * accel * flightTime * flightTime
      }
    })

    pos1d.map(x => Vec3.mul(dir, x))
  }


  def drawRoughFlight(
      ship: Spacecraft,
      startLocName: String, endLocName: String,
      startLoc: OrbitalElementsEstimator, endLoc: OrbitalElementsEstimator,
      startDate: CalendarDateTime, endDate: CalendarDateTime): BufferedImage = {

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    val res = 1.0 / 24

    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask - see below

    /*
    val date1 = ticks.takeRight(1)(0)
    val date2 = ticks.last
    println("diff: ", date1 - date2)
    ticks.foreach(x => println(x.toDouble + "\t" + calendarDate(x.toDouble).dateString()))
    */

    val startLocStates = ticks.map(tick => Orbits.planetState(startLoc, tick))
    val endLocStates   = ticks.map(tick => Orbits.planetState(endLoc,   tick))

    ///

    val imWidth = 800
    val imHeight = 600

    val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
    val gr = im.getGraphics();
    gr.setColor(Color.BLACK)

    val allStates = startLocStates ++ endLocStates
    val planetMax = allStates.map(x => math.max(math.max(math.abs(x.position.x), math.abs(x.position.y)), math.abs(x.position.z))).max

    val camOrient = Vec3(math.Pi * 0.25, 0, math.Pi)
    val camPos = Vec3(0, -planetMax * 2.25, planetMax * 2.25)
    val camTrans = View.cameraTransform(camOrient, camPos)
    val viewPos = Vec3(-imWidth * 0.1, 0, imWidth * 1.0)
    val view = new Viewer(camTrans, viewPos)
    val gridLim = (planetMax * 4).toInt

    val (flightStates, accel) = roughFlightGivenTime(
        startLocStates.head.position,
        endLocStates.last.position,
        endDateJulian - startDateJulian, res)

    val distance = Vec3.length(Vec3.sub(endLocStates.last.position, startLocStates.head.position))
    // val distanceGm = distance * Conversions.AuToMeters / Conversions.DayToSec / 1e9


    val vel = distance / (endDateJulian - startDateJulian) // average velocity
    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    val accelG = accel * aud2ToMs2 / Conversions.GToMetersPerSecond
    val shipAccelG = ship.accel * aud2ToMs2 / Conversions.GToMetersPerSecond

    val thrust = ship.mass * accel
    val thrustKN = thrust * aud2ToMs2
    val shipThrustKN = ship.thrust * aud2ToMs2

    gr.fillRect(0, 0, imWidth, imHeight)

    view.drawGrid(im, gridLim, new Color(0, 0, 80))

    val startLocFullPeriod = Orbits.planetMotionPeriod(startLoc, startDateJulian)
    val endLocFullPeriod = Orbits.planetMotionPeriod(endLoc, startDateJulian)

    view.drawMotion(im, Points3d(startLocFullPeriod.map(_.position)), Color.GRAY)
    view.drawMotion(im, Points3d(endLocFullPeriod.map(_.position)),   Color.GRAY)

    view.drawMotion(im, Points3d(startLocStates.map(_.position)), Color.GREEN)
    view.drawMotion(im, Points3d(endLocStates.map(_.position)),   Color.GREEN)
    view.drawMotion(im, Points3d(flightStates),                   Color.CYAN)

    def drawArrow(os: OrbitalState, color: Color): Unit = {
      val position = View.perspective(os.position, camTrans, viewPos)
      val direction = Vec2.normalize(Vec2.sub(
          View.perspective(Vec3.add(os.position, os.velocity), camTrans, viewPos),
          position))
      val arrowPoints = Viewer.arrowPoints(position, direction)
      view.drawPolygon(im, arrowPoints, color)
    }

    val arrowIndex1 = startLocFullPeriod.length / 4
    val arrowIndex2 = arrowIndex1 * 3

    drawArrow(startLocFullPeriod(arrowIndex1), Color.GRAY)
    drawArrow(startLocFullPeriod(arrowIndex2), Color.GRAY)
    drawArrow(endLocFullPeriod(arrowIndex1),   Color.GRAY)
    drawArrow(endLocFullPeriod(arrowIndex2),   Color.GRAY)

    println("starting position velocity:" + Vec3.length(startLocStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(endLocStates.last.velocity) + " AU/day")

    view.drawPosition(im, startLocStates.head.position, startLocName, startDate.dateString, Color.GREEN)
    view.drawPosition(im, endLocStates.last.position,   endLocName,   endDate.dateString,   Color.GREEN)
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw a text description
    val columnWidth = 100
    val tableStartX = Viewer.LineHeight
    val tableStartY = Viewer.LineHeight * 2

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN): Unit = {
      gr.setColor(Color.GREEN)
      gr.drawString(desc, tableStartX, tableStartY + row * Viewer.LineHeight)
      gr.setColor(color)
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + columnWidth, tableStartY + (row + x._2) * Viewer.LineHeight)
      })
    }

    gr.setFont(Viewer.DisplayFont)

    val reqColor = if (ship.accel > accel) {
      Color.GREEN
    } else {
      Color.RED
    }

    table("Spacecraft:", Seq(ship.name), 0)
    table("Mass:",       Seq("%.2f".format(ship.mass)    + " tonnes"), 1)
    table("a max:",      Seq("%.4f".format(ship.accel)   + " AU/day²",
                             "%.4f".format(shipAccelG)   + " g" ), 2)
    table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   4)

    table("Departure:", Seq(startDate.dateString + " " + startLocName ), 6)
    table("Arrival:",   Seq(endDate.dateString   + " " + endLocName),    7)
    table("Distance:",  Seq(f"$distance%.4f AU"), 8)
    table("v mean:",    Seq(f"$vel%.4f AU/day",
                            f"$velKmPerSec%.4f km/s",
                            f"$velC%.4f C"), 9)
    table("a req:",     Seq(f"$accel%.4f AU/day²",
                            f"$accelG%.4f g"), 12, reqColor)
    table("f req:",     Seq(f"$thrustKN%.2f kN"), 14, reqColor)

    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

    im

  }


}
