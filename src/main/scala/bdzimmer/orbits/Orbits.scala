// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Experimentation with calculating and visualizing positions of planets and paths
// between them.

// mostly based on these:
// http://www.braeunig.us/space/plntpos.htm
// http://www.stargazing.net/kepler/ellipse.html#twig02a
// https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf

package bdzimmer.orbits

import scala.sys.process._
import scala.collection.immutable.Seq

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO


case class CalendarDateTime(
    year: Int, month: Int, day: Int,
    hours: Int = 0, minutes: Int = 0, seconds: Double = 0.0) {

  def dateTimeString(): String = {
    val secondsInt = seconds.toInt
    f"$year%04d-$month%02d-$day%02d $hours%02d:$minutes%02d:$secondsInt%02d"
  }

  def dateString(): String = {
    val secondsInt = seconds.toInt
    f"$year%04d-$month%02d-$day%02d"
  }

}


object Orbits {

  val MetersInAu = 1.49597870700e11
  val SecInDay = 86400
  val LightMetersPerSec = 299792458.0

  val JGREG = 15 + 31* ( 10 + 12 * 1582)

  def julianDate(date: CalendarDateTime): Double = {

    val day = date.day + (date.hours / 24.0) + (date.minutes / 1440.0) + date.seconds

    val (yearMod, monthMod) = if (date.month < 3) {
      (date.year - 1, date.month + 12)
    } else {
      (date.year, date.month)
    }

    val b = if (day + 31 * (monthMod + 12 * yearMod) >= JGREG) {
      val a = math.floor(yearMod / 100)
      2 - a + math.floor(a / 4)
    } else {
      0.0
    }

    math.floor(365.25 * yearMod) + math.floor(30.6001 * (monthMod + 1)) + day + 1720994.5 + b

  }


  def calendarDate(date: Double): CalendarDateTime = {
    // https://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_day_number
    val y = 4716
    val j = 1401
    val m = 2
    val n = 12
    val r = 4
    val p = 1461
    val v = 3
    val u = 5
    val s = 153
    val w = 2
    val B = 274277
    val C = -38

    val J = math.floor(date + 0.5).toInt
    val f = J + j + (((4 * J + B) / 146097) * 3)  / 4 + C
    val e = r * f + v
    val g = (e % p) / r
    val h = u * g + w
    val D = (h % s) / u + 1
    val M = ((h / s + m) % n) + 1
    val Y = (e / p) - y + (n + m - M) / n

    // TODO: hours, minutes, and seconds
    CalendarDateTime(Y, M, D)
  }


  // estimate the true anomaly using Equation of the Center
  // TODO: solve iteratively using Newton's method
  def trueAnomaly(oe: OrbitalElements): Double = {

    val e = oe.eccentricity
    val e2 = e * e
    val e3 = e * e * e

    (oe.meanAnomaly
       + ( (2 * e             - (1/4) * e3) * math.sin(    oe.meanAnomaly)
         + (      (5/4) * e2              ) * math.sin(2 * oe.meanAnomaly)
         + (                  (13/12) * e3) * math.sin(3 * oe.meanAnomaly)))
  }


  def radius(oe: OrbitalElements, trueAnomaly: Double): Double = {
    val e = oe.eccentricity
    val e2 = e * e
    oe.semimajorAxis * (1 - e2) / (1 + e * math.cos(trueAnomaly))
  }


  // these functions work, but I'm not currently using them
  /*
  def planetXYZ(oee: OrbitalElementsEstimator, t: Double): Vec3d = {
    val oe = oee(t)
    val v  = trueAnomaly(oe)
    val r  = radius(oe, v)
    cartesianCoords(oe, v, r)
  }


  def cartesianCoords(oe: OrbitalElements, v: Double, r: Double): Vec3d = {

    val i = oe.inclination
    val o = oe.longitudeAscending  // uppercase omega
    val p = oe.longitudePeriapsis  // pi

    val x = r * (math.cos(o) * math.cos(v + p - o) - math.sin(o) * math.sin(v + p - o) * math.cos(i))
    val y = r * (math.sin(o) * math.cos(v + p - o) + math.cos(o) * math.sin(v + p - o) * math.cos(i))
    val z = r * (math.sin(v + p - o) * math.sin(i))

    Vec3d(x, y, z)
  }
  *
  */


  def positionOrbital(oe: OrbitalElements, v: Double, r: Double): Vec3 = {
    Vec3(r * math.cos(v), r * math.sin(v), 0.0)
  }


  def positionOrbital(oe: OrbitalElements): Vec3 = {
    val v = trueAnomaly(oe)
    val r = radius(oe, v)
    positionOrbital(oe, v, r)
  }


  // analytical method to find velocity - didn't seem to work
  /*
  def velocityOrbital(oe: OrbitalElements, v: Double, r: Double, mu: Double): Vec3d = {

    val e = oe.eccentricity
    val e2 = e * e

    val eccAnomaly = math.atan2(
        math.sqrt(1 - e2) * math.sin(v),
        e + math.cos(v))

    val coeff = math.sqrt(mu * oe.semimajorAxis) / r

    Vec3d(
        coeff * -math.sin(eccAnomaly),
        coeff * math.sqrt(1 - e2) * math.cos(eccAnomaly),
        0.0)
  }
  *
  */


  def transformOrbitalInertial(p: Vec3, oe: OrbitalElements): Vec3 = {

    val sinw = math.sin(oe.argPeriapsis)
    val cosw = math.cos(oe.argPeriapsis)

    val sino = math.sin(oe.longitudeAscending)
    val coso = math.cos(oe.longitudeAscending)

    val sini = math.sin(oe.inclination)
    val cosi = math.cos(oe.inclination)

    Vec3(
        p.x * (cosw * coso - sinw * cosi * sino) - p.y * (sinw *        coso + cosw * cosi * sino),
        p.x * (cosw * sino + sinw * cosi * coso) + p.y * (cosw * cosi * coso - sinw *        sino),
        p.x * (sinw * sini)                      + p.y * (cosw * sini))

  }


  def planetState(oee: OrbitalElementsEstimator, t: Double, dt: Double = 0.0001): OrbitalState = {
    val oeT1 = oee(t)
    val posOrbT1 = positionOrbital(oeT1)

    val oeT2 = oee(t + dt)
    val posOrbT2 = positionOrbital(oeT2)

    val velOrb = Vec3(
        (posOrbT2.x - posOrbT1.x) / dt,
        (posOrbT2.y - posOrbT1.y) / dt,
        (posOrbT2.z - posOrbT1.z) / dt
    )

    OrbitalState(
        transformOrbitalInertial(posOrbT1, oeT1),
        transformOrbitalInertial(velOrb,   oeT1))
  }


  def planetFullPeriod(oee: OrbitalElementsEstimator, startTime: Double, nPoints: Int = 365): Seq[OrbitalState] = {
    // Apply Kepler's Third Law to find the period of the planet. Assumes the planet is orbiting the sun.
    val oeStartTime = oee(startTime)
    val r3 = oeStartTime.semimajorAxis * oeStartTime.semimajorAxis * oeStartTime.semimajorAxis
    val period = math.sqrt(365.0 * 365.0 * r3)
    val timeInterval = period / nPoints
    val times = (0 to nPoints).map(t => startTime + t * timeInterval)
    times.map(t => planetState(oee, t))
  }


  def stateString(name: String, jd: Double, os: OrbitalState): String = {
    val pos = os.position.x + ", " + os.position.y + ", " + os.position.z
    val vel = os.velocity.x + ", " + os.velocity.y + ", " + os.velocity.z
    name + ", " + jd + ", " + pos + ", " + vel
  }


  def roughFlightGivenTime(startPos: Vec3, endPos: Vec3, flightTime: Double, res: Double): Seq[Vec3] = {

    val path = Vec3.sub(endPos, startPos)
    val pathLength = Vec3.length(path)
    val dir = Vec3(path.x / pathLength, path.y / pathLength, path.z / pathLength)

    val accel = 4 * pathLength / (flightTime * flightTime)
    roughFlight(dir, accel, flightTime, res).map(x => Vec3.add(startPos, x))

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
      startLocName: String, endLocName: String,
      startLoc: OrbitalElementsEstimator, endLoc: OrbitalElementsEstimator,
      startDate: CalendarDateTime, endDate: CalendarDateTime): BufferedImage = {

    val startDateJulian = julianDate(startDate)
    val endDateJulian = julianDate(endDate)

    val res = 1.0 / 24

    val ticks = (startDateJulian to endDateJulian by res).toList.toIndexedSeq // don't ask - see below

    /*
    val date1 = ticks.takeRight(1)(0)
    val date2 = ticks.last
    println("diff: ", date1 - date2)
    ticks.foreach(x => println(x.toDouble + "\t" + calendarDate(x.toDouble).dateString()))
    */

    val startLocStates = ticks.map(tick => planetState(startLoc, tick))
    val endLocStates   = ticks.map(tick => planetState(endLoc,   tick))

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
    val viewPos = Vec3(0, 0, imWidth * 1.0)
    val view = new View(camTrans, viewPos)
    val gridLim = (planetMax * 4).toInt

    val flightStates = roughFlightGivenTime(
        startLocStates.head.position,
        endLocStates.last.position,
        endDateJulian - startDateJulian, res)

    val distance = Vec3.length(Vec3.sub(endLocStates.last.position, startLocStates.head.position))

    val velAuPerDay = distance / (endDateJulian - startDateJulian)
    val velMetersPerSec = velAuPerDay * MetersInAu / SecInDay
    val velC = velMetersPerSec / LightMetersPerSec

    println("start: " + startLocStates.last.position)
    println("end: "   + endLocStates.last.position)

    println("distance: " + distance)
    println("average velocity: " + velAuPerDay + " AU/day")
    println("average velocity: " + velMetersPerSec + " m/s")
    println("average velocity: " + velC + " C")

    gr.fillRect(0, 0, imWidth, imHeight)

    view.drawGrid(im, gridLim, Color.BLUE)

    val startLocFullPeriod = planetFullPeriod(startLoc, startDateJulian)
    val endLocFullPeriod = planetFullPeriod(endLoc, startDateJulian)

    view.drawMotion(im, Points3d(startLocFullPeriod.map(_.position)), Color.GRAY)
    view.drawMotion(im, Points3d(endLocFullPeriod.map(_.position)),   Color.GRAY)

    view.drawMotion(im, Points3d(startLocStates.map(_.position)), Color.GREEN)
    view.drawMotion(im, Points3d(endLocStates.map(_.position)),   Color.GREEN)
    view.drawMotion(im, Points3d(flightStates),                   Color.CYAN)

    def drawArrow(os: OrbitalState, color: Color): Unit = {
      val arrowPoints = view.arrowPoints(
        View.perspective(os.position, camTrans, viewPos),
        Vec2.normalize(View.perspective(os.velocity, camTrans, viewPos)))
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

    im

  }


  def main(args: Array[String]): Unit = {

    val startDate = CalendarDateTime(2016, 7, 24, 0)
    val endDate   = CalendarDateTime(2016, 7, 28, 0)

    val junk = Vec2(1.0, 2.0)

    val startPlanet = MeeusPlanets.Earth
    val startPlanetName = "Earth"
    val endPlanet = MeeusPlanets.Mars
    val endPlanetName = "Mars"

    val im = drawRoughFlight(
        startPlanetName, endPlanetName, startPlanet, endPlanet, startDate, endDate)

    val outputImage = new java.io.File("output.png");
    ImageIO.write(im, "png", outputImage)

    // for visualization with other programs

    val outputFilename = "test.csv"
    val outputFile = new File(outputFilename)
    val pw = new java.io.PrintWriter(outputFile)

    val startDateJulian = julianDate(startDate)
    val endDateJulian = julianDate(endDate)

    for (tick <- startDateJulian until endDateJulian by (1.0 / 24)) {

      val earthState = planetState(startPlanet, tick)
      pw.println(stateString(startPlanetName, tick, earthState))

      val marsState = planetState(endPlanet, tick)
      pw.println(stateString(endPlanetName, tick, marsState))

    }

    pw.close()

  }

}
