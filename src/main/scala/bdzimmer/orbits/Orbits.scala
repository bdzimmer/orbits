// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Calculating and positions of planets and paths between them.

// Mostly based on these:
// http://www.braeunig.us/space/plntpos.htm
// http://www.stargazing.net/kepler/ellipse.html#twig02a
// https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf

package bdzimmer.orbits

import scala.collection.immutable.Seq
import scala.sys.process._
import scala.util.Try


sealed abstract class Spacecraft {
  val name: String
}


case class ConstAccelCraft (
    name:  String,
    mass:  Double,       // metric tons
    accel: Double        // AU / day^2
  ) extends Spacecraft {
  val thrust = mass * accel   // F = ma
}


case class ConstVelCraft (
  name: String,
  vel:  Double          // AU / day
) extends Spacecraft


object Conversions {

  val AuToMeters = 1.49597870700e11
  val DayToSec = 86400.0
  val LightToMetersPerSec = 299792458.0
  val GToMetersPerSecond = 9.80665

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

    val calendarDate = CalendarDateTime(Y, M, D)

    // TODO: calculate this without a conversion
    val time = (date - calendarDate.julian) * 86400.0
    val hours = (time / 3600.0).toInt
    val minutes = ((time - hours * 3600.0) / 60.0).toInt
    val seconds = time - hours * 3600.0 - minutes * 60.0

    // println(time + " " + hours + " " + minutes + " " + seconds)
    CalendarDateTime(Y, M, D, hours, minutes, seconds)
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
      + ((2 * e             - (1/4) * e3) * math.sin(    oe.meanAnomaly)
      +  (      (5/4) * e2              ) * math.sin(2 * oe.meanAnomaly)
      +  (                  (13/12) * e3) * math.sin(3 * oe.meanAnomaly)))
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
    val oeStartTime = oee(startTime)
    // val r3 = oeStartTime.semimajorAxis * oeStartTime.semimajorAxis * oeStartTime.semimajorAxis
    // val period = math.sqrt(365.0 * 365.0 * r3)
    val period = planetPeriod(oeStartTime.semimajorAxis)
    val timeInterval = period / nPoints
    val times = (0 to nPoints).map(t => startTime + t * timeInterval)
    times.map(t => planetState(oee, t))
  }

  def planetPeriod(semimajorAxis: Double): Double = {
    // Apply Kepler's Third Law to find the full period of a planet.
    // Assumes the planet is orbiting the sun.
    val periodOfEarth = 365.25
    val r3 = semimajorAxis * semimajorAxis * semimajorAxis
    math.sqrt(periodOfEarth * periodOfEarth * r3)
  }


  def stateString(name: String, jd: Double, os: OrbitalState): String = {
    val pos = os.position.x + ", " + os.position.y + ", " + os.position.z
    val vel = os.velocity.x + ", " + os.velocity.y + ", " + os.velocity.z
    name + ", " + jd + ", " + pos + ", " + vel
  }


}
