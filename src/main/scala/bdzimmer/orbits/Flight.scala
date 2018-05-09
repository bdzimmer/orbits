// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Basic interplanetary flight planning.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import scala.sys.process._
import scala.util.Try

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import bdzimmer.util.StringUtils._


// given a time, return position vector
abstract class FlightFn {
  def apply(t: Double): Vec3
}


class RoughFlightFn(
    startPos: Vec3,
    dir: Vec3,
    val accel: Double,
    val startTime: Double,
    flightTime: Double) extends FlightFn {

  val halfFlightTime = flightTime / 2

  def apply(t_abs: Double): Vec3 = {

    val t = t_abs - startTime

    if (t < 0.0 || t > startTime + flightTime) {
      Vec3(0.0, 0.0, 0.0)
    } else {
      val pos1d = if (t < halfFlightTime) {
        0.5 * accel * t * t
      } else {
        -0.5 * accel * t * t + accel * flightTime * t - 0.25 * accel * flightTime * flightTime
      }
      Vec3.add(startPos, Vec3.mul(dir, pos1d))
    }
  }

}


object RoughFlightFn {

  def apply(
      startPos: Vec3, endPos: Vec3, startTime: Double, flightTime: Double): RoughFlightFn = {

    val path = Vec3.sub(endPos, startPos)
    val pathLength = Vec3.length(path)
    val dir = Vec3(path.x / pathLength, path.y / pathLength, path.z / pathLength)
    val accel = 4 * pathLength / (flightTime * flightTime)

    new RoughFlightFn(startPos, dir, accel, startTime, flightTime)
  }

}


object SolveFlight {
  // WIP solve departure time given arrival time or vice versa

  val Eps = 1.0e-8
  val GuessDelta = 14.0 // two weeks
  val IterMax = 100

  // find end date of a flight, keeping accel less than ship's maximum accel
  def endDate(
    ship: Spacecraft,
    orig: Vec3,
    destFunc: Double => Vec3,
    startDate: Double,
    guessDelta: Double = GuessDelta): Option[Double] = {

    // TODO: if no result, search the next GuessDelta increment?
    search(
      startDate, startDate + guessDelta,
      x => math.abs(RoughFlightFn(orig, destFunc(x), startDate, x - startDate).accel - ship.accel))
  }

  // find start date of of the flight, keeping accel less than ship's maximum accel
  def startDate(
    ship: Spacecraft,
    origFunc: Double => Vec3,
    dest: Vec3,
    endDate: Double,
    guessDelta: Double = GuessDelta): Option[Double] = {

    search(
      endDate - guessDelta, endDate,
      x => math.abs(RoughFlightFn(origFunc(x), dest, x, endDate - x).accel - ship.accel))

  }


  // find a zero of a convex function on a bounded interval via a method like
  // binary search but where the decision to update the lower or upper bound
  // is made based on the sign of the slope

  // TODO: find minimum rather than zero
  // deal with possibility that minimum is not zero elsewhere

  def search(
    lowerBound: Double, upperBound: Double, func: Double => Double,
    eps: Double = Eps, iterMax: Int = IterMax): Option[Double] = {
    var lb = lowerBound
    var ub = upperBound
    // println(lb + "\t" + ub)
    for (iter <- 0 until iterMax) {
      val x = (lb + ub) * 0.5
      val y = func(x)
      val dy = func(x + eps) - y
      if (math.abs(y) > eps) {
        if (dy > 0.0) {
          ub = x
        } else {
          lb = x
        }
      } else {
        return Some(x)
      }
      println(iter + "\t" + y + "\t" + x + "\t" + lb + "\t" + ub)
    }
    None
  }

}


object RenderFlight {

  def drawRoughFlight(
      ship: Spacecraft,
      faction: String,
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

    val roughFlightFn =  RoughFlightFn(
      origStates.head.position, destStates.last.position,
      startDateJulian, endDateJulian - startDateJulian)
    val flightStates = ticks.map(tick => roughFlightFn(tick))

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    val vel = distance / (endDateJulian - startDateJulian) // average velocity

    ////

    val imWidth = 800
    val imHeight = 600

    val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
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
    drawFlightSummary(
        im, ship, faction, distance, vel, roughFlightFn.accel, origName, destName, startDate, endDate)

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

    val roughFlightFn =  RoughFlightFn(
      origStates.head.position, destStates.last.position,
      startDateJulian, endDateJulian - startDateJulian)
    val flightStates = ticks.map(tick => roughFlightFn(tick))

    val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

    val vel = distance / (endDateJulian - startDateJulian) // average velocity

    ////

    val imWidth = 800
    val imHeight = 600

    val allStates = origStates ++ destStates
    val planetMax = maxPosition(allStates)

    val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
    val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)

    val gridLim = (planetMax * 4).toInt

    for (idx <- 0 until flightStates.length) {

      println(idx + " / " + flightStates.length)

      val curState = flightStates(idx)

      val im = new BufferedImage(imWidth, imHeight, BufferedImage.TYPE_INT_ARGB)
      val gr = im.getGraphics.asInstanceOf[Graphics2D]
      gr.setRenderingHints(Viewer.RenderHints)
      gr.setColor(Color.BLACK)
      gr.fillRect(0, 0, imWidth, imHeight)

      // camera placed at 2 AU directly above the sun
      val camPos = Vec3(0, 0, 2)

      // find angles to point camera at ship
      val camToShip = Vec2(curState.x - camPos.x, curState.y - camPos.y)
      val camAngleX = math.atan2(curState.z - camPos.z, Vec2.length(camToShip))
      val camAngleZ = math.atan2(camToShip.x, camToShip.y)

      val camOrient = Vec3(-math.Pi * 0.5 - camAngleX, 0.0, math.Pi - camAngleZ)
      val camRot = View.rotationZYX(camOrient)

      val camTrans = View.cameraTransform(camRot, camPos)
      val xshiftAmount =  -imWidth * 0.1
      val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)

      val view = new Viewer(camTrans, viewPos)

      val curDateTime = Conversions.julianToCalendarDate(ticks(idx))

      drawRoughFlightAtTime(
        view,
        im,
        List((origName, origFullPeriod), (destName, destFullPeriod)),
        origName, destName,
        "", "",
        origStates.take(idx + 1),
        destStates.take(idx + 1),
        flightStates.take(idx + 1),
        Color.CYAN, // TODO: faction color
        List(),
        gridLim)

      // draw flight status and ship velocity arrow

      val curDist = Vec3.length(
        Vec3.sub(curState, origStates.head.position)) // ship is traveling in a straight line

      val curVel = if (idx > 1) {
        val curVelVec = Vec3.sub(curState, flightStates(idx - 1))
        val res = Vec3.length(curVelVec)
        if (res > 1.0e-9) {
          view.drawArrow(im, OrbitalState(curState, curVelVec), Color.GREEN)
        }
        res
      } else {
        0.0
      }

      drawFlightStatus(im, ship, "", curDateTime, curDist, curVel)

      val outputImage = new java.io.File(outputDir / f"$idx%05d.png");
      ImageIO.write(im, "png", outputImage)

    }

    // print some things for debugging
    println("starting position velocity:" + Vec3.length(origStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(destStates.last.velocity) + " AU/day")
    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

  }


  def drawRoughFlightAtTime(
      view: Viewer,
      im: BufferedImage,
      planets: List[(String, Seq[OrbitalState])],
      origName: String,
      destName: String,
      origDesc: String,
      destDesc: String,
      origStates: Seq[OrbitalState],
      destStates: Seq[OrbitalState],
      flightStates: Seq[Vec3],
      flightColor: Color,
      otherFlights: List[(Seq[Vec3], Color)],
      gridLim: Int): Unit = {

    // draw the grid and the sun
    view.drawGrid(im, gridLim, new Color(0, 0, 80))
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw the orbits of planets and their positions
    // the sequence of orbital states for each planet should start from same time
    // as the final state of the flight - this is the time that we are drawing the
    // flight at
    planets.foreach(x => drawOrbit(im, x._2, view))
    planets.foreach(x => view.drawPosition(im, x._2.head.position, x._1, "", Color.GRAY))

    // draw other flights in the background
    // TODO: optional ship arrows and names
    otherFlights.foreach(x => view.drawMotion(im, x._1, x._2))

    // draw the positions of the start and ending locations and the flight up to this point
    view.drawMotion(im, origStates.map(_.position), Color.GREEN)
    view.drawMotion(im, destStates.map(_.position), Color.GREEN)
    view.drawMotion(im, flightStates,               flightColor)

    // draw the names and dates for the origin and destination
    view.drawPosition(im, origStates.head.position, origName, origDesc, Color.GREEN)
    view.drawPosition(im, destStates.last.position, destName, destDesc, Color.GREEN)

  }


  def imagesToVideo(inputDir: String, outputFile: String, width: Int, height: Int): Unit = {
    val command = s"ffmpeg -y -r 24 -f image2 -s ${width}x$height -i $inputDir/%05d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p $outputFile"
    println(command)
    Try(command.!!)
  }


  /// ///


  def maxPosition(states: Seq[OrbitalState]): Double = {
    states.map(x => math.max(math.max(math.abs(x.position.x), math.abs(x.position.y)), math.abs(x.position.z))).max
  }


  private def defaultViewParams(
      imWidth: Int, imHeight: Int, planetMax: Double, xshift: Boolean = true): (Mat44, Vec3) = {

    val camOrient = Vec3(-math.Pi * 0.25, 0, math.Pi)
    val camPos = Vec3(0, -planetMax * 2.25, planetMax * 2.25)
    val camTrans = View.cameraTransform(View.rotationZYX(camOrient), camPos)
    val xshiftAmount = if (xshift) {
      -imWidth * 0.1
    } else {
      0.0
    }
    val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)

    (camTrans, viewPos)
  }


  def defaultView(imWidth: Int, imHeight: Int, planetMax: Double, xshift: Boolean = true): Viewer = {
    val (camTrans, viewPos) = defaultViewParams(imWidth, imHeight, planetMax, xshift)
    new Viewer(camTrans, viewPos)
  }


  def drawOrbit(
      im: BufferedImage, fullPeriod: Seq[OrbitalState],
      view: Viewer, color: Color = Color.GRAY): Unit = {

    // draw an orbit using arrows

    view.drawMotion(im, fullPeriod.map(_.position), Color.GRAY)

    val arrowIndex = fullPeriod.length / 4
    view.drawArrow(im, fullPeriod(arrowIndex), Color.GRAY)
    view.drawArrow(im, fullPeriod(arrowIndex * 3), Color.GRAY)

  }


  def drawFlightSummary(
      im: BufferedImage,
      ship: Spacecraft,
      faction: String,
      distance: Double,
      vel: Double,
      accel: Double,
      origName: String,
      destName: String,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime): Unit = {

    // draw a table describing a summary of the flight

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

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(Viewer.DisplayFont)
      gr.drawString(desc, tableStartX, tableStartY + row * Viewer.LineHeight)
      gr.setColor(color)
      if (italic) {
        gr.setFont(Viewer.DisplayFontItalic)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + columnWidth, tableStartY + (row + x._2) * Viewer.LineHeight)
      })
    }

    val reqColor = if (ship.accel > accel) {
      Color.GREEN
    } else {
      Color.RED
    }

    table("Spacecraft:", Seq(ship.name.replace("*", "")), 0, italic = true)
    table("Faction:",    Seq(faction), 1)
    table("Mass:",       Seq("%.2f".format(ship.mass)    + " tonnes"), 2)
    table("a max:",      Seq("%.4f".format(ship.accel)   + " AU/day²",
                             "%.4f".format(shipAccelG)   + " g" ), 3)
    table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   5)

    table("Departure:", Seq(startDate.dateTimeString + " " + origName), 7)
    table("Arrival:",   Seq(endDate.dateTimeString   + " " + destName), 8)
    table("Distance:",  Seq(f"$distance%.4f AU"), 9)
    table("v mean:",    Seq(f"$vel%.4f AU/day",
                            f"$velKmPerSec%.4f km/s",
                            f"$velC%.4f C"), 10)
    table("a req:",     Seq(f"$accel%.4f AU/day²",
                            f"$accelG%.4f g"), 13, reqColor)
    table("f req:",     Seq(f"$thrustKN%.2f kN"), 15, reqColor)

  }


  def drawFlightStatus(
      im: BufferedImage,
      ship: Spacecraft,
      faction: String,
      dateTime: CalendarDateTime,
      distance: Double,
      vel: Double): Unit = {

    // draw a table describing the current flight status

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    val shipAccelG = ship.accel * aud2ToMs2 / Conversions.GToMetersPerSecond

    val shipThrustKN = ship.thrust * aud2ToMs2

    val columnWidth = 100
    val tableStartX = Viewer.LineHeight
    val tableStartY = Viewer.LineHeight * 2

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(Viewer.DisplayFont)
      gr.drawString(desc, tableStartX, tableStartY + row * Viewer.LineHeight)
      gr.setColor(color)
      if (italic) {
        gr.setFont(Viewer.DisplayFontItalic)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + columnWidth, tableStartY + (row + x._2) * Viewer.LineHeight)
      })
    }

    table("Spacecraft:", Seq(ship.name.replace("*", "")), 0, italic = true)
    table("Faction:",    Seq(faction), 1)
    table("Mass:",       Seq("%.2f".format(ship.mass)    + " tonnes"), 2)
    table("a max:",      Seq("%.4f".format(ship.accel)   + " AU/day²",
                             "%.4f".format(shipAccelG)   + " g" ), 3)
    table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   5)

    table("DateTime:",  Seq(dateTime.dateTimeString), 7)
    table("Distance:",  Seq(f"$distance%.4f AU"), 8)
    table("v:",         Seq(f"$vel%.4f AU/day",
                            f"$velKmPerSec%.4f km/s",
                            f"$velC%.4f C"), 9)
  }

}
