// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Experimentation with calculating positions of planets.

// mostly based on these:
// http://www.braeunig.us/space/plntpos.htm
// http://www.stargazing.net/kepler/ellipse.html#twig02a
// https://downloads.rene-schwarz.com/download/M001-Keplerian_Orbit_Elements_to_Cartesian_State_Vectors.pdf

// 2016-06-28 to 2016-07-01

package bdzimmer.orbits

import java.io.File
import scala.sys.process._


case class Polynomial4(
    a0: Double = 0.0,
    a1: Double = 0.0,
    a2: Double = 0.0,
    a3: Double = 0.0) {
  def apply(t: Double): Double = {
    a0 + a1 * t + a2 * t * t + a3 * t * t * t;
  }
}


case class Vec3d(x: Double, y: Double, z: Double) {
  def length(): Double = {
    math.sqrt(x * x + y * y + z * z)
  }
}


case class OrbitalState(position: Vec3d, velocity: Vec3d)


case class OrbitalElements(
  
  // L - mean longitude of the planet
  // plane is from the vernal equinox along the ecliptic to the orbit's ascending node
  longitudeMean:      Double,
  semimajorAxis: Double, // a - semimajor axis of the orbit
  eccentricity:  Double, // e - eccentricity of the orbit
  inclination:   Double, // i - inclination on the plane of the ecliplic
  argPeriapsis: Double, // lowercase omega - argument of periapsis
  longitudeAscending: Double,  // uppercase omega - longitude of ascending node
  
  // pi - longitude of periapsis
  longitudePeriapsis: Double,
  // M - mean anomaly
  meanAnomaly: Double,
  // periapsis distance
  rp: Double,
  // apoapsis distance
  ra: Double
  
)


sealed abstract class OrbitalElementsEstimator {
  def apply(t: Double): OrbitalElements
}


class NonEarthPolynomialEstimator(
    longitudeMean: Polynomial4,
    semimajorAxis: Double,
    eccentricity:  Polynomial4,
    inclination:   Polynomial4,
    argPeriapsis:  Polynomial4,
    longitudeAscending: Polynomial4   
) extends OrbitalElementsEstimator {
  
  def apply(t: Double): OrbitalElements = {
    val x = (t - 2415020.0) / 36525
    
    val longMean = longitudeMean(x)  * math.Pi / 180.0
    val ecc = eccentricity(x)
    val inc = inclination(x) * math.Pi / 180
    val argPeri = argPeriapsis(x) * math.Pi / 180.0
    val longAsc = longitudeAscending(x) * math.Pi / 180.0
    val longPeri = argPeri + longAsc
    val meanAnomaly = longMean - longPeri
    val rp = semimajorAxis * (1 - ecc)
    val ap = semimajorAxis * (1 + ecc)
    
    OrbitalElements(
        longMean, semimajorAxis, ecc, inc, argPeri, longAsc,
        longPeri, meanAnomaly, rp, ap)
        
  }
  
}


class EarthPolynomialEstimator(
    longitudeMean: Polynomial4,
    eccentricity:  Polynomial4,
    meanAnomaly:   Polynomial4
) extends OrbitalElementsEstimator {
  
  def apply(t: Double): OrbitalElements = {
    val x = (t - 2415020.0) / 36525
    val semimajorAxis = 1.0000002
    
    val longMean = longitudeMean(x) * math.Pi / 180.0
    val ecc = eccentricity(x) 
    val meanAnom = meanAnomaly(x) * math.Pi / 180.0
    val longAscending = 0.0
    val longPeri = longMean - meanAnom
    val argPeri = longPeri // longPeri - longAscending (longAscending = 0.0)
    val rp = semimajorAxis * (1 - ecc)
    val ap = semimajorAxis * (1 + ecc)
    
    OrbitalElements(
        longMean,  semimajorAxis, ecc, 0.0, argPeri, longAscending,
        longPeri, meanAnom, rp, ap)
        
  }
  
}


object Orbits {
  
  val JGREG = 15 + 31*( 10 + 12 * 1582);
  
  val Earth = new EarthPolynomialEstimator(
      Polynomial4(99.69668, 36000.76892, 0.0003025),
      Polynomial4(0.01675104, -0.0000418, -0.000000126),
      Polynomial4(358.47583, 35999.04975, -0.000150, -0.0000033))
      
  val Mars = new NonEarthPolynomialEstimator(
      Polynomial4(293.737334, 19141.69551, 0.0003107),
      1.5236883,
      Polynomial4(0.09331290, 0.000092064, -0.000000077),	 
      Polynomial4(1.850333,  -0.0006750,    0.0000126),
      Polynomial4(285.431761, 1.0697667,    0.0001313,  0.00000414),
 	    Polynomial4(48.786442,  0.7709917,    0.0000014, -0.000005330))
  
  val Uranus = new NonEarthPolynomialEstimator(
      Polynomial4(244.197470, 429.863546, 0.0003160, -0.00000060),
      19.21814, 	 	 
      Polynomial4(0.0463444, -0.00002658, 0.000000077),
      Polynomial4(0.772464,   0.0006253,  0.0000395),
      Polynomial4(98.071581,  0.9857650,  0.0010745, -0.00000061),
      Polynomial4(73.477111,  0.4986678,  0.0013117))
  
  
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
  
  
  def positionOrbital(oe: OrbitalElements, v: Double, r: Double): Vec3d = {
    Vec3d(r * math.cos(v), r * math.sin(v), 0.0)
  }
  
  
  def positionOrbital(oe: OrbitalElements): Vec3d = {
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
  
  
  def transformOrbitalInertial(p: Vec3d, oe: OrbitalElements): Vec3d = {
  
    val sinw = math.sin(oe.argPeriapsis)
    val cosw = math.cos(oe.argPeriapsis)
    
    val sino = math.sin(oe.longitudeAscending)
    val coso = math.cos(oe.longitudeAscending)
    
    val sini = math.sin(oe.inclination)
    val cosi = math.cos(oe.inclination)
    
    Vec3d(
        p.x * (cosw * coso - sinw * cosi * sino) - p.y * (sinw *        coso + cosw * cosi * sino),
        p.x * (cosw * sino + sinw * cosi * coso) + p.y * (cosw * cosi * coso - sinw *        sino),
        p.x * (sinw * sini)                      + p.y * (cosw * sini))
        
    
  }
  

  def planetState(oee: OrbitalElementsEstimator, t: Double, dt: Double = 0.0001): OrbitalState = {
    val oeT1 = oee(t)
    val posOrbT1 = positionOrbital(oeT1)
    
    val oeT2 = oee(t + dt)
    val posOrbT2 = positionOrbital(oeT2)
    
    val velOrb = Vec3d(
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
      
      
      val earthState = planetState(Earth, jd)
      pw.println(stateString("Earth", jd, earthState))
      
      val marsState = planetState(Mars, jd)
      pw.println(stateString("Mars", jd, marsState))
      
      val uranusState = planetState(Uranus, jd)
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
