// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Experimentation with calculating positions of planets.

// mostly based on these:
// http://www.braeunig.us/space/plntpos.htm
// http://www.stargazing.net/kepler/ellipse.html#twig02a
// https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf

package bdzimmer.orbits

import java.io.File
import scala.sys.process._


object Orbits {

  val JGREG = 15 + 31* ( 10 + 12 * 1582)

  def julianDate(year: Int, month: Int, day: Double): Double = {

    val (yearMod, monthMod) = if (month < 3) {
      (year - 1, month + 12)
    } else {
      (year, month)
    }

    val b = if (day + 31 * (monthMod + 12 * yearMod) >= JGREG) {
      val a = math.floor(yearMod / 100)
      2 - a + math.floor(a / 4 )
    } else {
      0.0
    }

    math.floor(365.25 * yearMod) + math.floor(30.6001 * (monthMod + 1)) + day + 1720994.5 + b

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


  def stateString(name: String, jd: Double, os: OrbitalState): String = {
    val pos = os.position.x + ", " + os.position.y + ", " + os.position.z
    val vel = os.velocity.x + ", " + os.velocity.y + ", " + os.velocity.z
    name + ", " + jd + ", " + pos + ", " + vel
  }


  def main(args: Array[String]): Unit = {
    val startDate = julianDate(2016, 6, 1)

    val metersInAu = 1.49597870700e11
    val secInDay = 86400
    // val earthGm = 3.986005e14 / (metersInAu * metersInAu * metersInAu) * secInDay * secInDay

    val outputFilename = "test.csv"
    val outputFile = new File(outputFilename)
    val pw = new java.io.PrintWriter(outputFile)

    for (tick <- 0 until 365 * 84) {

      val jd = startDate + tick

      val earthState = planetState(MeeusPlanets.Earth, jd)
      pw.println(stateString("Earth", jd, earthState))

      val marsState = planetState(MeeusPlanets.Mars, jd)
      pw.println(stateString("Mars", jd, marsState))

      val uranusState = planetState(MeeusPlanets.Uranus, jd)
      pw.println(stateString("Uranus", jd, uranusState))

      /*
      val velAu = earthState.velocity.length
      println("velocity in AU / D: " + velAu)
      println("velocity in M / S : " + velAu * metersInAu / secInDay)
      println("***")
      */

    }

    pw.close()

    // println("Working directory: " + System.getProperty("user.dir"))

    // this is just for testing - it won't work to run from the JAR
    Seq("Rscript", "src/main/resources/orbits/orbits.R", s"${outputFile.getAbsolutePath}").run()


  }

}
