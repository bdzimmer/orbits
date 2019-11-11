// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Basic interplanetary flight planning.

package bdzimmer.orbits

import scala.collection.immutable.Seq
import scala.sys.process._
import scala.util.Try
import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage

import bdzimmer.orbits.Moons.{MoonICRFEstimator}
import javax.imageio.ImageIO
import bdzimmer.util.StringUtils._


// given a time, return position vector
abstract class FlightFn {
  val startTime: Double
  def apply(t: Double): Vec3
}


class ConstAccelFlightFn(
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


object ConstAccelFlightFn {

  def apply(
      startPos: Vec3, endPos: Vec3, startTime: Double, flightTime: Double): ConstAccelFlightFn = {

    val path = Vec3.sub(endPos, startPos)
    val pathLength = Vec3.length(path)
    val dir = Vec3(path.x / pathLength, path.y / pathLength, path.z / pathLength)
    val accel = 4 * pathLength / (flightTime * flightTime)

    new ConstAccelFlightFn(startPos, dir, accel, startTime, flightTime)
  }

}


class ConstVelFlightFn(
  startPos: Vec3,
  dir: Vec3,
  val vel: Double,
  val startTime: Double,
  flightTime: Double) extends FlightFn {

  def apply(t_abs: Double): Vec3 = {
    val t = t_abs - startTime
    if (t < 0.0 || t > startTime + flightTime) {
      Vec3(0.0, 0.0, 0.0)
    } else {
      val pos1d = t * vel
      Vec3.add(startPos, Vec3.mul(dir, pos1d))
    }
  }
}


object ConstVelFlightFn {

  val VelMin = 1.0e-9

  def apply(
      startPos: Vec3, endPos: Vec3, startTime: Double, flightTime: Double): ConstVelFlightFn = {

    val path = Vec3.sub(endPos, startPos)
    val pathLength = Vec3.length(path)
    val dir = Vec3(path.x / pathLength, path.y / pathLength, path.z / pathLength)
    val vel = pathLength / flightTime

    new ConstVelFlightFn(startPos, dir, vel, startTime, flightTime)
  }

}


object SolveFlight {
  // WIP solve departure time given arrival time or vice versa

  val Eps = 1.0e-8
  val GuessDelta = 14.0 // two weeks
  val IterMax = 100

  // solvers that switch based on ship type

  def endDate(
      ship: Spacecraft,
      orig: Vec3,
      destFunc: Double => Vec3,
      startDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {
    ship match {
      case x: ConstAccelCraft => constAccelEndDate(x, orig, destFunc, startDate, guessDelta)
      case x: ConstVelCraft   =>   constVelEndDate(x, orig, destFunc, startDate, guessDelta)
    }
  }

  def startDate(
      ship: Spacecraft,
      origFunc: Double => Vec3,
      dest: Vec3,
      endDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {
    ship match {
      case x: ConstAccelCraft => constAccelStartDate(x, origFunc, dest, endDate, guessDelta)
      case x: ConstVelCraft   =>   constVelStartDate(x, origFunc, dest, endDate, guessDelta)
    }
  }

  // solvers for constant acceleration

  // find end date of a flight, keeping accel less than ship's maximum accel
  def constAccelEndDate(
      ship: ConstAccelCraft,
      orig: Vec3,
      destFunc: Double => Vec3,
      startDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {

    search(
      startDate, startDate + guessDelta,
      x => math.abs(ConstAccelFlightFn(orig, destFunc(x), startDate, x - startDate).accel - ship.accel))
  }

  // find start date of of the flight, keeping accel less than ship's maximum accel
  def constAccelStartDate(
      ship: ConstAccelCraft,
      origFunc: Double => Vec3,
      dest: Vec3,
      endDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {

    search(
      endDate - guessDelta, endDate,
      x => math.abs(ConstAccelFlightFn(origFunc(x), dest, x, endDate - x).accel - ship.accel))
  }

  // solvers for constant velocity

  // find end date of a flight, keeping vel less than ship's maximum vel
  def constVelEndDate(
      ship: ConstVelCraft,
      orig: Vec3,
      destFunc: Double => Vec3,
      startDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {

    search(
      startDate, startDate + guessDelta,
      x => math.abs(ConstVelFlightFn(orig, destFunc(x), startDate, x - startDate).vel - ship.vel))
  }

  // find start date of of the flight, keeping vel less than ship's maximum vel
  def constVelStartDate(
      ship: ConstVelCraft,
      origFunc: Double => Vec3,
      dest: Vec3,
      endDate: Double,
      guessDelta: Double = GuessDelta): Option[Double] = {

    search(
      endDate - guessDelta, endDate,
      x => math.abs(ConstVelFlightFn(origFunc(x), dest, x, endDate - x).vel - ship.vel))
  }

  // find a zero of a convex function on a bounded interval via a method like
  // binary search but where the decision to update the lower or upper bound
  // is made based on the sign of the slope

  // TODO: find minimum rather than zero
  // deal with possibility that minimum is not zero elsewhere

  def search(
    lowerBound: Double, upperBound: Double, func: Double => Double,
    diffEps: Double = Eps, zeroEps: Double = Eps * 1000.0, iterMax: Int = IterMax): Option[Double] = {
    var lb = lowerBound
    var ub = upperBound
    var x = (lb + ub) * 0.5
    // println(lb + "\t" + ub)
    for (iter <- 0 until iterMax) {
      x = (lb + ub) * 0.5
      val y = func(x)
      val dy = func(x + diffEps) - y
      if (math.abs(y) > zeroEps) {
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

  val ColorOrbital = new Color(0, 200, 0, 127)
  val ColorLaplace = new Color(200, 0, 0, 127)

  def drawFlight(
      ship: Spacecraft,
      faction: String,
      origName: String,
      destName: String,
      orig: OrbitalElementsEstimator,
      dest: OrbitalElementsEstimator,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime): BufferedImage = {

    val viewerSettings = Viewer.ViewerSettingsDefault

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origStates = ticks.map(tick => Orbits.planetState(orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(dest, tick))

    ///

    val flightFn = ship match {
      case _: ConstAccelCraft => ConstAccelFlightFn(
        origStates.head.position, destStates.last.position, startDateJulian, endDateJulian - startDateJulian)
      case _: ConstVelCraft => ConstVelFlightFn(
        origStates.head.position, destStates.last.position, startDateJulian, endDateJulian - startDateJulian)
    }

    val flightStates = ticks.map(tick => flightFn(tick))

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
    view.drawGrid(im, gridLim, gridLim, None, new Color(0, 0, 80))

    // draw the full periods of the starting and ending locations
    val origFullPeriod = Orbits.planetMotionPeriod(orig, startDateJulian)
    val destFullPeriod = Orbits.planetMotionPeriod(dest, startDateJulian)
    drawOrbit(im, origFullPeriod, view, Color.GRAY, false)
    drawOrbit(im, destFullPeriod, view, Color.GRAY, false)

    // draw the positions of the start and ending locations and the flight
    view.drawMotion(im, origStates.map(_.position), Color.GREEN, true, false)
    view.drawMotion(im, destStates.map(_.position), Color.GREEN, true, false)
    view.drawMotion(im, flightStates,               Color.CYAN, true, false)

    // draw the names and dates for start and end
    view.drawPosition(im, origStates.head.position, origName, startDate.dateString, Color.GREEN)
    view.drawPosition(im, destStates.last.position, destName, endDate.dateString,   Color.GREEN)
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw flight summary
    ship match {
      case x: ConstAccelCraft => {
        val accel = flightFn match {
          case y: ConstAccelFlightFn => y.accel
          case _ => 0.0
        }
        drawFlightSummary(
          im, x, faction, distance, vel, accel, origName, destName, startDate, endDate, viewerSettings)
      }
      case x: ConstVelCraft => drawFlightSummary(
        im, x, faction, distance, vel, origName, destName, startDate, endDate, viewerSettings)
    }

    // print some things for debugging
    println("starting position velocity:" + Vec3.length(origStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(destStates.last.velocity) + " AU/day")
    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

    // finished!

    im

  }


  def animateFlight(
      ship: Spacecraft,
      origName: String,
      destName: String,
      orig: OrbitalElementsEstimator,
      dest: OrbitalElementsEstimator,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      outputDir: String): Unit = {

    val viewerSettings = Viewer.ViewerSettingsDefault

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    // one tick per hour
    val res = 1.0 / 24
    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask

    // find positions of origin and destination bodies
    val origStates = ticks.map(tick => Orbits.planetState(orig, tick))
    val destStates = ticks.map(tick => Orbits.planetState(dest, tick))

    ///

    val flightFn = ship match {
      case _: ConstAccelCraft => ConstAccelFlightFn(
        origStates.head.position, destStates.last.position, startDateJulian, endDateJulian - startDateJulian)
      case _: ConstVelCraft => ConstVelFlightFn(
        origStates.head.position, destStates.last.position, startDateJulian, endDateJulian - startDateJulian)
    }

    val flightStates = ticks.map(tick => flightFn(tick))

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
      val camRot = Transformations.rotationZYX(camOrient)

      val camTrans = View.cameraTransform(camRot, camPos)
      val xshiftAmount =  -imWidth * 0.1
      val viewPos = Vec3(xshiftAmount, 0, imWidth * 1.0)

      val view = new Viewer(camTrans, viewPos, Viewer.ViewerSettingsDefault)

      val curDateTime = Conversions.julianToCalendarDate(ticks(idx))

      drawFlightAtTime(
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

      val curVel = if (idx > 0) {
        val curVelVec = Vec3.sub(curState, flightStates(idx - 1))
        val res = Vec3.length(curVelVec)
        if (res > ConstVelFlightFn.VelMin) {
          view.drawArrow(im, OrbitalState(curState, curVelVec), Color.GREEN)
        }
        res
      } else {
        0.0
      }

      drawFlightStatus(im, ship, "", curDateTime, curDist, curVel, viewerSettings)

      val outputImage = new java.io.File(outputDir / f"$idx%05d.png");
      ImageIO.write(im, "png", outputImage)

    }

    // print some things for debugging
    println("starting position velocity:" + Vec3.length(origStates.head.velocity) + " AU/day")
    println("ending position velocity:  " + Vec3.length(destStates.last.velocity) + " AU/day")
    println("distance: " + distance)
    println("average velocity: " + vel + " AU/day")

  }


  def drawFlightAtTime(
      view: Viewer,
      im: BufferedImage,
      planets: Seq[(String, Seq[OrbitalState])],
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

    drawStateAtTime(view, im, planets, otherFlights, gridLim)

    drawHighlightedFlightAtTime(
      view, im, origName, destName, origDesc, destDesc, origStates, destStates, flightStates, flightColor)
  }


  def drawStateAtTime(
       view: Viewer,
       im: BufferedImage,
       planets: Seq[(String, Seq[OrbitalState])],
       otherFlights: List[(Seq[Vec3], Color)],
       gridLim: Int): Unit = {

    // draw the grid and the sun
    view.drawGrid(im, gridLim, gridLim, None, new Color(0, 0, 128))
    view.drawPosition(im, Vec3(0.0, 0.0, 0.0), "Sun", "", Color.YELLOW) // for now

    // draw the orbits of planets and their positions
    // the sequence of orbital states for each planet should start from same time
    // as the final state of the flight - this is the time that we are drawing the
    // flight at
    planets.foreach(x => drawOrbit(im, x._2, view, Color.GRAY, false))
    planets.foreach(x => view.drawPosition(im, x._2.head.position, x._1, "", Color.GRAY))

    // draw other flights in the background
    // TODO: optional ship arrows and names
    otherFlights.foreach(x => view.drawMotion(im, x._1, x._2, true, false))
  }

  def highlightFlight(
      view: Viewer,
      im: BufferedImage,
      flight: FlightParams,
      factions: Map[String, Color],

      origStates: Seq[OrbitalState],
      destStates: Seq[OrbitalState],
      flightStates: Seq[Vec3],
      flightColor: Color): Unit = {

    // to plot how the origin and desination change curing the flight

    RenderFlight.drawHighlightedFlightAtTime(
      view, im,
      flight.origName, flight.destName,
      flight.startDate.dateString, flight.endDate.dateString,
      origStates, destStates, flightStates, flightColor)

  }


  def drawHighlightedFlightAtTime(
       view: Viewer,
       im: BufferedImage,
       origName: String,
       destName: String,
       origDesc: String,
       destDesc: String,
       origStates: Seq[OrbitalState],
       destStates: Seq[OrbitalState],
       flightStates: Seq[Vec3],
       flightColor: Color): Unit = {

    // draw the positions of the start and ending locations and the flight up to this point
    view.drawMotion(im, origStates.map(_.position), flightColor, true, false)
    view.drawMotion(im, destStates.map(_.position), flightColor, true, false)
    view.drawMotion(im, flightStates,               flightColor, true, false)

    // draw the names and dates for the origin and destination
    // view.drawPosition(im, origStates.head.position, origName, origDesc, Color.GREEN)
    // view.drawPosition(im, destStates.last.position, destName, destDesc, Color.GREEN)
  }


  def imagesToVideo(inputDir: String, outputFile: String, width: Int, height: Int, fps: Int): Unit = {
    val command = s"ffmpeg -y -r $fps -f image2 -s ${width}x$height -i $inputDir/%05d.png -threads 2 -vcodec libx264 -crf 25 -pix_fmt yuv420p $outputFile"
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
    val camTrans = View.cameraTransform(Transformations.rotationZYX(camOrient), camPos)
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
    new Viewer(camTrans, viewPos, Viewer.ViewerSettingsDefault)
  }


  def drawOrbit(
      im: BufferedImage,
      fullPeriod: Seq[OrbitalState],
      view: Viewer,
      color: Color,
      verticals: Boolean,
      adjustAlpha: Boolean = true): Unit = {

    // draw an orbit using arrows
    view.drawMotion(im, fullPeriod.map(_.position), color, true, verticals, adjustAlpha)
    val arrowIndex = fullPeriod.length / 4

    if (!adjustAlpha) {
      view.drawArrow(im, fullPeriod(arrowIndex), color)
      view.drawArrow(im, fullPeriod(arrowIndex * 3), color)
    } else {
      val qColor = new Color(
        color.getRed, color.getGreen, color.getBlue,
        (255.0 * 0.25).toInt)
      val tqColor = new Color(
        color.getRed, color.getGreen, color.getBlue,
        (255.0 * 0.75).toInt)
      view.drawArrow(im, fullPeriod(arrowIndex), qColor)
      view.drawArrow(im, fullPeriod(arrowIndex * 3), tqColor)
    }

  }


  def drawFlightSummary(
      im: BufferedImage,
      ship: ConstAccelCraft,
      faction: String,
      distance: Double,
      vel: Double,
      accel: Double,
      origName: String,
      destName: String,
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      viewerSettings: ViewerSettings): Unit = {

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

    // val columnWidth = 100
    val tableStartX = viewerSettings.lineHeight
    val tableStartY = viewerSettings.lineHeight * 2

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(viewerSettings.displayFont)
      gr.drawString(desc, tableStartX, tableStartY + row * viewerSettings.lineHeight)
      gr.setColor(color)
      if (italic) {
        gr.setFont(viewerSettings.displayFontItalic)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + viewerSettings.columnWidth, tableStartY + (row + x._2) * viewerSettings.lineHeight)
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

    table("Departure:",  Seq(startDate.dateTimeString + " " + origName), 7)
    table("Arrival:",    Seq(endDate.dateTimeString   + " " + destName), 8)
    table("Distance:",   Seq(f"$distance%.4f AU"), 9)
    table("v mean:",     Seq(f"$vel%.4f AU/day",
                                f"$velKmPerSec%.4f km/s",
                                f"$velC%.4f C"), 10)
    table("a req:",      Seq(f"$accel%.4f AU/day²",
                                f"$accelG%.4f g"), 13, reqColor)
    table("f req:",      Seq(f"$thrustKN%.2f kN"), 15, reqColor)

  }


  def drawFlightSummary(
    im: BufferedImage,
    ship: ConstVelCraft,
    faction: String,
    distance: Double,
    vel: Double,
    origName: String,
    destName: String,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    viewerSettings: ViewerSettings): Unit = {

    // draw a table describing a summary of the flight

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val shipVelMetersPerSec = ship.vel * Conversions.AuToMeters / Conversions.DayToSec
    val shipVelKmPerSec = shipVelMetersPerSec / 1000.0
    val shipVelC = velMetersPerSec / Conversions.LightToMetersPerSec

    // val columnWidth = 100
    val tableStartX = viewerSettings.lineHeight
    val tableStartY = viewerSettings.lineHeight * 2

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(viewerSettings.displayFont)
      gr.drawString(desc, tableStartX, tableStartY + row * viewerSettings.lineHeight)
      gr.setColor(color)
      if (italic) {
        gr.setFont(viewerSettings.displayFontItalic)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + viewerSettings.columnWidth, tableStartY + (row + x._2) * viewerSettings.lineHeight)
      })
    }

    val reqColor = if (ship.vel > vel) {
      Color.GREEN
    } else {
      Color.RED
    }

    table("Spacecraft:", Seq(ship.name.replace("*", "")), 0, italic = true)
    table("Faction:",    Seq(faction), 1)
    table("v max:",      Seq("%.4f".format(ship.vel)    + " AU/day²",
                                "%.4f".format(shipVelKmPerSec) + " km/s",
                                "%.4f".format(shipVelC)        + " C"), 2)
    table("Departure:",  Seq(startDate.dateTimeString + " " + origName), 5)
    table("Arrival:",    Seq(endDate.dateTimeString   + " " + destName), 6)
    table("Distance:",   Seq(f"$distance%.4f AU"), 7)
    table("v req:",     Seq(f"$vel%.4f AU/day",
                               f"$velKmPerSec%.4f km/s",
                               f"$velC%.4f C"), 9, reqColor)
  }


  def drawFlightStatus(
    im: BufferedImage,
    ship: Spacecraft,
    faction: String,
    dateTime: CalendarDateTime,
    distance: Double,
    vel: Double,
    viewerSettings: ViewerSettings): Unit = {

    // draw a table describing the current flight status

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    val massString = ship match {
      case x: ConstAccelCraft => "%.2f".format(x.mass)
      case _ => "undefined"
    }

    val shipAccel = ship match {
      case x: ConstAccelCraft => x.accel
      case _ => 0.0
    }
    val shipAccelG = shipAccel * aud2ToMs2 / Conversions.GToMetersPerSecond

    val shipThrustKN = ship match {
      case x: ConstAccelCraft => x.thrust * aud2ToMs2
      case _ => 0.0
    }

    // val columnWidth = 100
    val tableStartX = viewerSettings.lineHeight
    val tableStartY = viewerSettings.lineHeight * 2

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(viewerSettings.displayFont)
      gr.drawString(desc, tableStartX, tableStartY + row * viewerSettings.lineHeight)
      gr.setColor(color)
      if (italic) {
        gr.setFont(viewerSettings.displayFontItalic)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(x._1, tableStartX + viewerSettings.columnWidth, tableStartY + (row + x._2) * viewerSettings.lineHeight)
      })
    }

    table("Spacecraft:", Seq(ship.name.replace("*", "")), 0, italic = true)
    table("Faction:",    Seq(faction), 1)
    table("Mass:",       Seq(massString    + " tonnes"), 2)
    table("a max:",      Seq("%.4f".format(shipAccel)   + " AU/day²",
                                "%.4f".format(shipAccelG)   + " g" ), 3)
    table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   5)

    table("DateTime:",   Seq(dateTime.dateTimeString), 7)
    table("Distance:",   Seq(f"$distance%.4f AU"), 8)
    table("v:",          Seq(f"$vel%.4f AU/day",
                                f"$velKmPerSec%.4f km/s",
                                f"$velC%.4f C"), 9)
  }


  // TODO: proper indentation
  def drawFlightStatus(
    tableStartX: Int,
    tableStartY: Int,
    im: BufferedImage,
    ship: Spacecraft,
    faction: String,
    factionColor: Color,
    dateTime: CalendarDateTime,
    distance: Double,
    vel: Double,
    viewerSettings: ViewerSettings): Unit = {

    // draw a table describing the current flight status

    val velMetersPerSec = vel * Conversions.AuToMeters / Conversions.DayToSec
    val velKmPerSec = velMetersPerSec / 1000.0
    // val velC = velMetersPerSec / Conversions.LightToMetersPerSec

    val aud2ToMs2 = Conversions.AuToMeters / (Conversions.DayToSec * Conversions.DayToSec)

    // val massString = ship match {
    //   case x: ConstAccelCraft => "%.2f".format(x.mass)
    //   case _ => "undefined"
    // }

    val shipAccel = ship match {
      case x: ConstAccelCraft => x.accel
      case _ => 0.0
    }
    // val shipAccelG = shipAccel * aud2ToMs2 / Conversions.GToMetersPerSecond

    // val shipThrustKN = ship match {
    //   case x: ConstAccelCraft => x.thrust * aud2ToMs2
    //   case _ => 0.0
    // }

    // val columnWidth = 100
    // val tableStartX = viewerSettings.lineHeight
    // val tableStartY = viewerSettings.lineHeight * 2

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    def table(desc: String, values: Seq[String], row: Int, color: Color = Color.GREEN, italic: Boolean = false): Unit = {
      gr.setColor(Color.GREEN)
      gr.setFont(viewerSettings.displayFontSmall)
      // gr.drawString(desc, tableStartX, tableStartY + row * viewerSettings.lineHeightSmall)
      gr.setColor(color)
      if (italic) {
        gr.setFont(viewerSettings.displayFontItalicSmall)
      }
      values.zipWithIndex.foreach(x => {
        gr.drawString(
          x._1,
          tableStartX, // + viewerSettings.columnWidthSmall,
          tableStartY + (row + x._2) * viewerSettings.lineHeightSmall + viewerSettings.lineHeightSmall)
      })
    }

    table("Spacecraft:", Seq(ship.name.replace("*", "")), 0, factionColor, italic = true)
    table("Faction:",    Seq(faction), 1, factionColor)
    // table("Mass:",       Seq(massString    + " tonnes"), 2)
    // table("a max:",      Seq("%.4f".format(shipAccel)   + " AU/day²",
    //   "%.4f".format(shipAccelG)   + " g" ), 3)
    // table("f max:",      Seq("%.2f".format(shipThrustKN) + " kN"),   5)

    // table("DateTime:",   Seq(dateTime.dateTimeString), 7)
    // table("Distance:",   Seq(f"$distance%.4f AU"), 8)
    table("v:",          Seq(f"$vel%.4f AU/day",
                                f"$velKmPerSec%.4f km/s"),
                                // f"$velC%.4f C"),
                               2,
                               factionColor)
  }


  def drawFlightRadii(
      im: BufferedImage,
      flight: FlightParams,
      curDateJulian: Double,
      view: Viewer): Unit = {

    // draw start, end, and current radius lines to start and end destinations

    val sun = Vec3(0.0, 0.0, 0.0)

    val startDateJulian = flight.startDate.julian
    val endDateJulian = flight.endDate.julian

    val origStart = Orbits.planetState(flight.orig, startDateJulian)
    // val origEnd = Orbits.planetState(flight.orig, endDateJulian)
    val origCur = Orbits.planetState(flight.orig, curDateJulian)

    // val destStart = Orbits.planetState(flight.dest, startDateJulian)
    val destEnd = Orbits.planetState(flight.dest, endDateJulian)
    val destCur = Orbits.planetState(flight.dest, curDateJulian)

    val color = new Color(255, 255, 255, 31)

    view.drawLine(im, sun, origStart.position, color)
    // view.drawLine(im, sun, origEnd.position, color)
    view.drawLine(im, sun, origCur.position, color)

    view.drawLine(im, sun, destCur.position, color)
    // view.drawLine(im, sun, destStart.position, color)
    view.drawLine(im, sun, destEnd.position, color)

  }


  def drawOrbitInfo(
      im: BufferedImage,
      oe: OrbitalElements,
      transformation: Mat44,
      view: Viewer): Unit = {

    // transformation is an additional transformation to be applied
    // for use with moons etc.

    // ~~~~ draw ascending node
    val color = new Color(255, 255, 255, 127)

    val centerTrans = Transformations.transform(transformation, Vec3(0.0, 0.0, 0.0))

    // argument of periapsis is angle forward from ascending node to periapsis (angle 0.0)
    val nodeAngle = 0.0 - oe.argPeriapsis
    val nodePos = Orbits.transformOrbitalInertial(
      Orbits.positionOrbital(oe, nodeAngle), oe)
    val nodePosTrans = Transformations.transform(transformation, nodePos)

    view.drawLine(im, centerTrans, nodePosTrans, color)
    view.drawLabel(im, "ascending node", dispAngle(nodeAngle), nodePosTrans, Color.GRAY)

    // ~~~ draw periapsis

    // periapsis is at angle 0.0
    val periAngle = 0.0
    val periPos = Orbits.transformOrbitalInertial(
      Orbits.positionOrbital(oe, periAngle),
      oe)
    val periPosTrans = Transformations.transform(transformation, periPos)

    view.drawLine(im, centerTrans, periPosTrans, color)
    view.drawLabel(im, "periapsis", dispAngle(periAngle), periPosTrans, Color.GRAY)

    // ~~~~ draw true anomaly

    val vAngle = Orbits.trueAnomaly(oe)
    val vPos = Orbits.transformOrbitalInertial(
      Orbits.positionOrbital(oe, vAngle),
      oe)
    val vPosTrans = Transformations.transform(transformation, vPos)

    view.drawLine(im, centerTrans, vPosTrans, color)
    view.drawLabel(im, "", dispAngle(vAngle), vPosTrans, Color.GRAY)

    // ~~~ draw grids for local Laplace plane and orbital plane

    // apoapsis at 180.0 degrees
    val radiusApoapsis = Orbits.radius(oe, math.Pi)
    val orbitalToIntertial = Transformations.transformation(
      Orbits.transformOrbitalInertial(oe), Vec3(0.0, 0.0, 0.0))

    // orbital plane and axis

    val transOrbital = transformation.mul(orbitalToIntertial)

    view.drawGrid(
      im,
      radiusApoapsis, 4,
      Some(transOrbital),
      ColorOrbital)

    view.drawLine(
      im,
      Transformations.transform(transOrbital, Vec3(0.0, 0.0, 0.0)),
      Transformations.transform(transOrbital, Vec3(0.0, 0.0, radiusApoapsis)),
      ColorOrbital)

    // local laplace plane and axis

    view.drawGrid(
      im,
      radiusApoapsis, 4,
      Some(transformation),
      ColorLaplace)

    view.drawLine(
      im,
      Transformations.transform(transformation, Vec3(0.0, 0.0, 0.0)),
      Transformations.transform(transformation, Vec3(0.0, 0.0, radiusApoapsis)),
      ColorLaplace)

  }


  // TODO: get rid of "transformation" argument
  def precessionPeriod(
      oee: MoonICRFEstimator,
      startTime: Double,
      transformation: Mat44): Seq[OrbitalState] = {

    val periodPnode = oee.pnode
    // val periodPw = oee.pw
    val period = periodPnode * Conversions.YearToDay

    val nPoints = 90
    val timeInterval = period / nPoints
    val times = (0 to nPoints).map(t => (startTime - period) + t * timeInterval)

    val positions = times.map(t => {
      val oe = oee(t)
      val radiusApoapsis = Orbits.radius(oe, math.Pi)
      val orbitalToIntertial = Transformations.transformation(
        Orbits.transformOrbitalInertial(oe), Vec3(0.0, 0.0, 0.0))
      val transOrbital = transformation.mul(orbitalToIntertial)
      Transformations.transform(transOrbital, Vec3(0.0, 0.0, radiusApoapsis))
    })

    // TODO: proper velocity calculation here
    positions.map(OrbitalState(_, Vec3(0.0, 0.0, 0.0)))

  }

  private def dispAngle(x: Double): String = {
    val angle = x / math.Pi
    val angleMod = angle % 2.0
    val angleModRound = math.rint(angleMod * 1000.0) / 1000.0
    angleModRound.toString + "\u03C0"
  }

}
