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


  def julian: Double = {

    val JGREG = 15 + 31* ( 10 + 12 * 1582)

    val dayFrac = day + (hours / 24.0) + (minutes / 1440.0) + seconds

    val (yearMod, monthMod) = if (month < 3) {
      (year - 1, month + 12)
    } else {
      (year, month)
    }

    val b = if (day + 31 * (monthMod + 12 * yearMod) >= JGREG) {
      val a = math.floor(yearMod / 100)
      2 - a + math.floor(a / 4)
    } else {
      0.0
    }

    math.floor(365.25 * yearMod) + math.floor(30.6001 * (monthMod + 1)) + day + 1720994.5 + b

  }

}

// mass in metric tons, thrust in metric tons * AU / day^2
case class Spacecraft(name: String, mass: Double, accel: Double) {
  // F = ma
  val thrust = mass * accel
}


object Conversions {

  val MetersInAu = 1.49597870700e11
  val SecInDay = 86400.0
  val LightMetersPerSec = 299792458.0

  def julianToCalendarDate(date: Double): CalendarDateTime = {
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

}


object Orbits {

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


  def planetMotionPeriod(oee: OrbitalElementsEstimator, startTime: Double, nPoints: Int = 365): Seq[OrbitalState] = {
    // Apply Kepler's Third Law to find the motion of a full period of a planet.
    // Assumes the planet is orbiting the sun.
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




  def main(args: Array[String]): Unit = {

    val compass = Spacecraft("EOE Compass", 30000.0, 0.2)
    val startDate = CalendarDateTime(2016, 7, 27, 0)
    val endDate   = CalendarDateTime(2016, 7, 31, 0)

    val startPlanet = MeeusPlanets.Mars
    val startPlanetName = "Mars"
    val endPlanet = MeeusPlanets.Earth
    val endPlanetName = "Earth"

    val im = Flight.drawRoughFlight(
        compass,
        startPlanetName, endPlanetName,
        startPlanet, endPlanet, startDate, endDate)

    val outputImage = new java.io.File("output.png");
    ImageIO.write(im, "png", outputImage)

    // for visualization with other programs

    val outputFilename = "test.csv"
    val outputFile = new File(outputFilename)
    val pw = new java.io.PrintWriter(outputFile)

    val startDateJulian = startDate.julian
    val endDateJulian = endDate.julian

    for (tick <- startDateJulian until endDateJulian by (1.0 / 24)) {

      val earthState = planetState(startPlanet, tick)
      pw.println(stateString(startPlanetName, tick, earthState))

      val marsState = planetState(endPlanet, tick)
      pw.println(stateString(endPlanetName, tick, marsState))

    }

    pw.close()

  }

}
