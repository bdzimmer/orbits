// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Calculating and positions of planets and paths between them.

// Mostly based on these:
// http://www.braeunig.us/space/plntpos.htm
// http://www.stargazing.net/kepler/ellipse.html#twig02a
// https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf

package bdzimmer.orbits

import bdzimmer.orbits.Moons.{MoonICRFEstimator}

import scala.collection.immutable.Seq


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

  val DegToRad = math.Pi / 180
  val AuToMeters = 1.49597870700e11
  val DayToSec = 86400.0
  val LightToMetersPerSec = 299792458.0
  val GToMetersPerSecond = 9.80665
  val YearToDay = 365.2425

  // TODO: proper conversion from ICRF to ecliptic coordinate system
  // val ICRFToEcliptic = Transformations.rotX(-23.4392811 * DegToRad) // original

  val correction = 3.0 * DegToRad

  val ICRFToEcliptic = (
      Transformations.rotZ(-correction).mul(
      Transformations.rotX(-23.4392811 * DegToRad).mul(
      Transformations.rotZ(correction))))


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


  def radius(oe: OrbitalElements, angle: Double): Double = {
    val e = oe.eccentricity
    val e2 = e * e
    oe.semimajorAxis * (1 - e2) / (1 + e * math.cos(angle))
  }


  def positionOrbital(oe: OrbitalElements, v: Double, r: Double): Vec3 = {
    Vec3(r * math.cos(v), r * math.sin(v), 0.0)
  }


  def positionOrbital(oe: OrbitalElements, v: Double): Vec3 = {
    val r = radius(oe, v)
    positionOrbital(oe, v, r)
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


  def transformOrbitalInertial(oe: OrbitalElements): Mat33 = {

    val sinw = math.sin(oe.argPeriapsis)
    val cosw = math.cos(oe.argPeriapsis)

    val sino = math.sin(oe.longitudeAscending)
    val coso = math.cos(oe.longitudeAscending)

    val sini = math.sin(oe.inclination)
    val cosi = math.cos(oe.inclination)

    val xNew = Vec3(
      cosw * coso - sinw * cosi * sino,
      cosw * sino + sinw * cosi * coso,
      sinw * sini)

    val yNew = Vec3(
      - (sinw * coso + cosw * cosi * sino),
      cosw * cosi * coso - sinw * sino,
      cosw * sini)

    Mat33(
      xNew,
      yNew,
      Vec3.cross(xNew, yNew)) // TODO: x cross y

  }




  def laplacePlaneICRFTransformation(rightAscension: Double, declination: Double): Mat33 = {
    // calculate transformation of laplace plane relative to ecliptic
    // from description of laplace plane relative to ICRF

    // TODO: sort of a guess for now

    // first rotate around Z by rightAscension
    // then rotate around X by declination
    // finally convert to ecliptic coordinate system
    val planeRelativeToICRF = radRotation(rightAscension, declination)

    val planeRelativeToEcliptic = Conversions.ICRFToEcliptic.mul(planeRelativeToICRF)
    // val planeRelativeToEcliptic = planeRelativeToICRF.mul(Conversions.ICRFToEcliptic)

    planeRelativeToEcliptic

  }

  def radRotation(rightAscension: Double, declination: Double): Mat33 = {
    // convert right ascention and declination of an axis orthogonal to a plane
    // to a transformation of the XY plane

    // original behavior
    // Transformations.rotX(declination).mul(Transformations.rotZ(rightAscension))

    // new behavior
    Transformations.rotZ(rightAscension).mul(Transformations.rotY(declination - 0.5 *  math.Pi))

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


  def planetMotionPeriod(
      oee: OrbitalElementsEstimator,
      startTime: Double,
      nPoints: Int = 365): Seq[OrbitalState] = {

    val oeStartTime = oee(startTime)
    val period = planetPeriod(oeStartTime.semimajorAxis)
    motionPeriod(oee, startTime, period, nPoints)

    // val timeInterval = period / nPoints
    // // calculate the period BEFORE the start date
    // val times = (0 to nPoints).map(t => (startTime - period) + t * timeInterval)
    // // confirm that the LAST tick is identical to the start date
    // // println(startTime + " " + times.last)
    // times.map(t => planetState(oee, t))
  }


  def moonMotionPeriod(
      primary: OrbitalElementsEstimator,
      moon: MoonICRFEstimator,
      laplacePlane: Option[Mat33],
      startTime: Double,
      nPoints: Int = 90
    ): Seq[OrbitalState] = {

    // TODO: is there a way to properly calculate the period instead of using this?
    val period = moon.p
    // val period = moon.pnode * Conversions.YearToDay
    // val period = moon.p * 12.0

    val moonRelMotion = Orbits.motionPeriod(moon, startTime, period, nPoints)

    // TODO: apply laplace plane transformation
    val moonRelMotionRot = laplacePlane.map(mat => {
      moonRelMotion.map(x =>  {
        OrbitalState(mat.mul(x.position), mat.mul(x.velocity))
      })
    }).getOrElse(moonRelMotion)

    // use this method if we want to plot the absolute position
    // of the moon over time below
    // val primaryMotion = Orbits.motionPeriod(primary, startTime, period, nPoints)
    // val earthState = primaryMotion.last

    val primaryState = planetState(primary, startTime)

    moonRelMotionRot.map(x => OrbitalState(
      Vec3.add(x.position, primaryState.position),
      x.velocity))

  }


  def motionPeriod(oee: OrbitalElementsEstimator, startTime: Double, period: Double, nPoints: Int = 365): Seq[OrbitalState] = {
    val timeInterval = period / nPoints
    // calculate the period BEFORE the start date
    val times = (0 to nPoints).map(t => (startTime - period) + t * timeInterval)
    // confirm that the LAST tick is identical to the start date
    // println(startTime + " " + times.last)
    times.map(t => planetState(oee, t))
  }


  def planetPeriod(semimajorAxis: Double): Double = {
    // Apply Kepler's Third Law to find (approximately) the full period of a planet.
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
