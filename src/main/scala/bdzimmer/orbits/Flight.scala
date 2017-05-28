// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Rough interplanetary flight planning.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import scala.sys.process._
import scala.util.Try

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import bdzimmer.util.StringUtils._


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
      origName: String,
      destName: String,
      orig: OrbitalElementsEstimator,
      dest: OrbitalElementsEstimator,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime): BufferedImage = {

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origStates = ticks.map(tick => Orbits.planetState(orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(dest, tick))

    ///

    val (flightStates, accel) = roughFlightGivenTime(
        origStates.head.position,
        destStates.last.position,
        endDateJulian - startDateJulian, res)

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    val vel = distance / (endDateJulian - startDateJulian) // average velocity

    ////

    val imWidth = 800
    val imHeight = 600

    val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)

    val gr = im.getGraphics()
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, imWidth, imHeight)

    val allStates = origStates ++ destStates
    val planetMax = maxPosition(allStates)
    val view = defaultView(imWidth, imHeight, planetMax)

    // draw the background grid
    val gridLim = (planetMax * 4).toInt
    view.drawGrid(im, gridLim, new Color(0, 0, 80))

    // draw the full periods of the starting and ending locations
    val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
    val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)
    drawOrbit(im, origFullPeriod, view)
    drawOrbit(im, destFullPeriod, view)

    // draw the positions of the start and ending locations and the flight
    view.drawMotion(im, origStates.map(_.position), Color.GREEN)
    view.drawMotion(im, destStates.map(_.position), Color.GREEN)
    view.drawMotion(im, flightStates,               Color.CYAN)

    // draw the names and dates for start and end
    view.drawPosition(im, origStates.head.position, origName, startDate.dateString, Color.GREEN)
    view.drawPosition(im, destStates.last.position, destName, endDate.dateString,   Color.GREEN)
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw flight summary
    drawFlightSummary(im, ship, distance, vel, accel, origName, destName, startDate, endDate)

    // print some things for debugging
    println("starting position velocity:" + Vec3.length(origStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(destStates.last.velocity) + " AU/day")
    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

    // finished!

    im

  }


  def animateRoughFlight(
      ship: Spacecraft,
      origName: String,
      destName: String,
      orig: OrbitalElementsEstimator,
      dest: OrbitalElementsEstimator,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      outputDir: String): Unit = {

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origStates = ticks.map(tick => Orbits.planetState(orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(dest, tick))

    ///

    val (flightStates, accel) = roughFlightGivenTime(
        origStates.head.position,
        destStates.last.position,
        endDateJulian - startDateJulian, res)

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    val vel = distance / (endDateJulian - startDateJulian) // average velocity

    ////

    val imWidth = 800
    val imHeight = 600

    val allStates = origStates ++ destStates
    val planetMax = maxPosition(allStates)

    /*
    val ti = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)

    val gr = ti.getGraphics()
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, imWidth, imHeight)

    val view = defaultView(imWidth, imHeight, planetMax)

    // draw the background grid
    val gridLim = (planetMax * 4).toInt
    view.drawGrid(ti, gridLim, new Color(0, 0, 80))

    // draw the full periods of the starting and ending locations
    val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
    val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)
    drawOrbit(ti, origFullPeriod, view)
    drawOrbit(ti, destFullPeriod, view)
    *
    */

    for (idx <- 0 until flightStates.length) {

      println(idx + " / " + flightStates.length)

      val curState = flightStates(idx)

      val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
      val gr = im.getGraphics
      // gr.drawImage(ti, 0, 0, null)
      gr.setColor(Color.BLACK)
      gr.fillRect(0, 0, imWidth, imHeight)

      val camPos = Vec3(curState.x, -planetMax * 1.50 + curState.y, planetMax * 2.25)

      val camOrient = Vec3(math.Pi * 0.25, 0.0, math.Pi)
      val camRot = View.rotation(camOrient)
      val camTrans = View.cameraTransform(camRot, camPos)
      val xshiftAmount =  -imWidth * 0.1
      val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)
      val view = new Viewer(camTrans, viewPos)

      val gridLim = (planetMax * 4).toInt
      view.drawGrid(im, gridLim, new Color(0, 0, 80))
      val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
      val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)
      drawOrbit(im, origFullPeriod, view)
      drawOrbit(im, destFullPeriod, view)

      val curDateTime = Conversions.julianToCalendarDate(ticks(idx))

      // draw the positions of the start and ending locations and the flight up to this point
      view.drawMotion(im, origStates.take(idx + 1).map(_.position), Color.GREEN)
      view.drawMotion(im, destStates.take(idx + 1).map(_.position), Color.GREEN)
      view.drawMotion(im, flightStates.take(idx + 1),               Color.CYAN)

      // draw the names and dates for current positions of origin and destination
      view.drawPosition(im, origStates(idx).position, origName, "", Color.GREEN)
      view.drawPosition(im, destStates(idx).position, destName, "", Color.GREEN)
      view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

      // draw flight status

      val curDist = Vec3.length(
        Vec3.sub(curState, origStates.head.position)) // ship is traveling in a straight line

      val curVel = if (idx > 0) {
        val curVelVec = Vec3.sub(curState, flightStates(idx - 1))
        val res = Vec3.length(curVelVec)
        if (res > 1.0e-9) {
          view.drawArrow(im, OrbitalState(curState, curVelVec), Color.GREEN)
        }
        res
      } else {
        0.0
      }

      drawFlightStatus(im, ship, curDateTime, curDist, curVel)

      val outputImage = new java.io.File(outputDir / f"$idx%05d.png");
      ImageIO.write(im, "png", outputImage)

    }

    // print some things for debugging
    println("starting position velocity:" + Vec3.length(origStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(destStates.last.velocity) + " AU/day")
    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

  }


  def imagesToVideo(inputDir: String, outputFile: String, width: Int, height: Int): Unit = {
    val command = s"ffmpeg -y -r 24 -f image2 -s ${width}x$height -i $inputDir/%05d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p $outputFile"
    println(command)
    Try(command.!!)
  }


  /// ///


  private def maxPosition(states: Seq[OrbitalState]): Double = {
    states.map(x => math.max(math.max(math.abs(x.position.x), math.abs(x.position.y)), math.abs(x.position.z))).max
  }


  private def defaultView(imWidth: Int, imHeight: Int, planetMax: Double, xshift: Boolean = true): Viewer = {
    val camOrient = Vec3(math.Pi * 0.25, 0, math.Pi)
    val camPos = Vec3(0, -planetMax * 2.25, planetMax * 2.25)
    val camTrans = View.cameraTransform(View.rotation(camOrient), camPos)
    val xshiftAmount = if (xshift) {
      -imWidth * 0.1
    } else {
      0.0
    }
    val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)
    new Viewer(camTrans, viewPos)
  }


  private def drawOrbit(
      im: BufferedImage, fullPeriod: Seq[OrbitalState],
      view: Viewer, color: Color = Color.GRAY): Unit = {

    // draw an orbit using arrows

    view.drawMotion(im, fullPeriod.map(_.position), Color.GRAY)

    val arrowIndex = fullPeriod.length / 4
    view.drawArrow(im, fullPeriod(arrowIndex), Color.GRAY)
    view.drawArrow(im, fullPeriod(arrowIndex * 3), Color.GRAY)

  }


  private def drawFlightSummary(
      im: BufferedImage,
      ship: Spacecraft,
      distance: Double,
      vel: Double,
      accel: Double,
      origName: String,
      destName: String,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime): Unit = {

    // draw a table describing various flight info

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    val accelG = accel * aud2ToMs2 / Conversions.GToMetersPerSecond
    val shipAccelG = ship.accel * aud2ToMs2 / Conversions.GToMetersPerSecond

    val thrust = ship.mass * accel
    val thrustKN = thrust * aud2ToMs2
    val shipThrustKN = ship.thrust * aud2ToMs2

    val columnWidth = 100
    val tableStartX = Viewer.LineHeight
    val tableStartY = Viewer.LineHeight * 2

    val gr = im.getGraphics

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

    table("Departure:", Seq(startDate.dateString + " " + origName), 6)
    table("Arrival:",   Seq(endDate.dateString   + " " + destName), 7)
    table("Distance:",  Seq(f"$distance%.4f AU"), 8)
    table("v mean:",    Seq(f"$vel%.4f AU/day",
                            f"$velKmPerSec%.4f km/s",
                            f"$velC%.4f C"), 9)
    table("a req:",     Seq(f"$accel%.4f AU/day²",
                            f"$accelG%.4f g"), 12, reqColor)
    table("f req:",     Seq(f"$thrustKN%.2f kN"), 14, reqColor)


  }


  private def drawFlightStatus(
      im: BufferedImage,
      ship: Spacecraft,
      dateTime: CalendarDateTime,
      distance: Double,
      vel: Double): Unit = {

    // draw a table describing various flight info

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    val shipAccelG = ship.accel * aud2ToMs2 / Conversions.GToMetersPerSecond

    val shipThrustKN = ship.thrust * aud2ToMs2

    val columnWidth = 100
    val tableStartX = Viewer.LineHeight
    val tableStartY = Viewer.LineHeight * 2

    val gr = im.getGraphics

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN): Unit = {
      gr.setColor(Color.GREEN)
      gr.drawString(desc, tableStartX, tableStartY + row * Viewer.LineHeight)
      gr.setColor(color)
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + columnWidth, tableStartY + (row + x._2) * Viewer.LineHeight)
      })
    }

    gr.setFont(Viewer.DisplayFont)

    table("Spacecraft:", Seq(ship.name), 0)
    table("Mass:",       Seq("%.2f".format(ship.mass)    + " tonnes"), 1)
    table("a max:",      Seq("%.4f".format(ship.accel)   + " AU/day²",
                             "%.4f".format(shipAccelG)   + " g" ), 2)
    table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   4)

    table("DateTime:",  Seq(dateTime.dateString), 6)
    table("Distance:",  Seq(f"$distance%.4f AU"), 7)
    table("v:",         Seq(f"$vel%.4f AU/day",
                            f"$velKmPerSec%.4f km/s",
                            f"$velC%.4f C"), 8)
  }





}
