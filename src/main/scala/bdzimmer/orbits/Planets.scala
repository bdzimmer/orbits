// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Calculate positions of planets.

// http://www.braeunig.us/space/plntpos.htm

package bdzimmer.orbits


case class OrbitalState(position: Vec3, velocity: Vec3)


case class OrbitalElements(

  // L - mean longitude of the planet
  // plane is from the vernal equinox along the ecliptic to the orbit's ascending node
  longitudeMean: Double,

  // a - semimajor axis of the orbit
  semimajorAxis: Double,

  // e - eccentricity of the orbit
  eccentricity:  Double,

  // i - inclination on the plane of the ecliplic
  inclination:   Double,

  // lowercase omega - argument of periapsis
  argPeriapsis:  Double,

  // uppercase omega - longitude of ascending node
  longitudeAscending: Double,

  // pi - longitude of periapsis
  longitudePeriapsis: Double,

  // M - mean anomaly
  meanAnomaly:        Double,

  // periapsis distance
  rp: Double,

  // apoapsis distance
  ra: Double

)


sealed abstract class OrbitalElementsEstimator {
  def apply(t: Double): OrbitalElements
}


// An implementation of the simple models for planet positions described here:
// http://www.braeunig.us/space/plntpos.htm
// Attributed to Jean Meeus' book Astronomical Formulae for Calculators, 4th ed.

object MeeusPlanets {

  // 0 January 1900 12h
  val J1900 = 2415020.0

  val DegToRad = math.Pi / 180


  val Mercury = new NonEarthPolynomialEstimator(
      Polynomial4(178.179078, 149474.07078, 0.0003011),
      0.3870986,
      Polynomial4(0.20561421, 0.00002046, -0.000000030),
      Polynomial4(7.002881,   0.0018608,  -0.0000183),
      Polynomial4(28.753753,  0.3702806,   0.0001208),
      Polynomial4(47.145944,  1.1852083,   0.0001739))

  val Earth = new EarthPolynomialEstimator(
      Polynomial4(99.69668,    36000.76892,  0.0003025),
      1.0000002,
      Polynomial4(0.01675104, -0.0000418,   -0.000000126),
      0.0,
      Polynomial4(358.47583,   35999.04975, -0.000150, -0.0000033))

  val Mars = new NonEarthPolynomialEstimator(
      Polynomial4(293.737334, 19141.69551,  0.0003107),
      1.5236883,
      Polynomial4(0.09331290, 0.000092064, -0.000000077),
      Polynomial4(1.850333,  -0.0006750,    0.0000126),
      Polynomial4(285.431761, 1.0697667,    0.0001313,  0.00000414),
      Polynomial4(48.786442,  0.7709917,    0.0000014, -0.000005330))

  val Saturn = new NonEarthPolynomialEstimator(
      Polynomial4(266.564377,  1223.509884,  0.0003245, -0.0000058),
      9.554747,
      Polynomial4(0.05589232, -0.00034550, -0.000000728, 0.00000000074),
      Polynomial4(2.492519,   -0.0039189,  -0.00001549,  0.00000004),
      Polynomial4(338.307800,  1.0852207,   0.00097854,  0.00000992),
      Polynomial4(112.790414,  0.8731951,  -0.00015218, -0.00000531))

  val Uranus = new NonEarthPolynomialEstimator(
      Polynomial4(244.197470, 429.863546, 0.0003160, -0.00000060),
      19.21814,
      Polynomial4(0.0463444, -0.00002658, 0.000000077),
      Polynomial4(0.772464,   0.0006253,  0.0000395),
      Polynomial4(98.071581,  0.9857650,  0.0010745, -0.00000061),
      Polynomial4(73.477111,  0.4986678,  0.0013117))


  val Planets = scala.collection.immutable.ListMap(
      "Mercury" -> Mercury,
      "Earth"   -> Earth,
      "Mars"    -> Mars,
      "Saturn"  -> Saturn,
      "Uranus"  -> Uranus)


  class NonEarthPolynomialEstimator(
    longitudeMean: Polynomial4,
    semimajorAxis: Double,
    eccentricity:  Polynomial4,
    inclination:   Polynomial4,
    argPeriapsis:  Polynomial4,
    longitudeAscending: Polynomial4
  ) extends OrbitalElementsEstimator {

    def apply(t: Double): OrbitalElements = {
      val x = (t - J1900) / 36525

      val longMean = longitudeMean(x)  * DegToRad
      val ecc = eccentricity(x)
      val inc = inclination(x) * DegToRad
      val argPeri = argPeriapsis(x) * DegToRad
      val longAsc = longitudeAscending(x) * DegToRad
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
      semimajorAxis: Double,
      eccentricity:  Polynomial4,
      inclination:   Double,
      meanAnomaly:   Polynomial4
  ) extends OrbitalElementsEstimator {

    def apply(t: Double): OrbitalElements = {
      val x = (t - J1900) / 36525

      val longMean = longitudeMean(x) * DegToRad
      val ecc = eccentricity(x)
      val meanAnom = meanAnomaly(x) * DegToRad
      val longAscending = 0.0
      val longPeri = longMean - meanAnom
      val argPeri = longPeri // longPeri - longAscending (longAscending = 0.0)
      val rp = semimajorAxis * (1 - ecc)
      val ap = semimajorAxis * (1 + ecc)

      OrbitalElements(
          longMean,  semimajorAxis, ecc, inclination, argPeri, longAscending,
          longPeri, meanAnom, rp, ap)
    }
  }

  case class Polynomial4(
    a0: Double = 0.0,
    a1: Double = 0.0,
    a2: Double = 0.0,
    a3: Double = 0.0) {

    def apply(t: Double): Double =  a0 + a1 * t + a2 * t * t + a3 * t * t * t;

  }


  class L3Estimator(oee: OrbitalElementsEstimator) extends OrbitalElementsEstimator {
    def apply(t: Double): OrbitalElements = {
      val oeeT = oee(t)
      val period = Orbits.planetPeriod(oeeT.semimajorAxis)
      oee(t + period / 2.0)
    }
  }


  class L4Estimator(oee: OrbitalElementsEstimator) extends OrbitalElementsEstimator {
    def apply(t: Double): OrbitalElements = {
      val oeeT = oee(t)
      val period = Orbits.planetPeriod(oeeT.semimajorAxis)
      oee(t + period / 6.0)
    }
  }


  class L5Estimator(oee: OrbitalElementsEstimator) extends OrbitalElementsEstimator {
    def apply(t: Double): OrbitalElements = {
      val oeeT = oee(t)
      val period = Orbits.planetPeriod(oeeT.semimajorAxis)
      oee(t - period / 6.0)
    }
  }

}
