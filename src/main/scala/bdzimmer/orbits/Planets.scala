// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Calculate positions of planets.

// http://www.braeunig.us/space/plntpos.htm

package bdzimmer.orbits


case class OrbitalState(position: Vec3, velocity: Vec3)


case class Planet(
  planet: OrbitalElementsEstimator,
  axialTilt: LaplacePlane,
  radiusKm: Double
)


class LaplacePlane(ra: Double, dec: Double) {
  val rightAscension: Double = ra * Conversions.DegToRad
  val declination: Double = dec * Conversions.DegToRad
}


case class OrbitalElements(

  // L - mean longitude of the planet
  // plane is from the vernal equinox along the ecliptic to the orbit's ascending node
  longitudeMean: Double,

  // a - semimajor axis of the orbit
  semimajorAxis: Double,

  // e - eccentricity of the orbit
  eccentricity:  Double,

  // i - inclination on the plane of the ecliptic
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

// Axial tilts from IAU section of https://en.wikipedia.org/wiki/Axial_tilt

object MeeusPlanets {

  // 0 January 1900 12h
  val J1900 = 2415020.0

  val Mercury = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(178.179078, 149474.07078, 0.0003011),
      0.3870986,
      Polynomial4(0.20561421, 0.00002046, -0.000000030),
      Polynomial4(7.002881,   0.0018608,  -0.0000183),
      Polynomial4(28.753753,  0.3702806,   0.0001208),
      Polynomial4(47.145944,  1.1852083,   0.0001739)),
    new LaplacePlane(281.01, 61.45),
    2439.7
  )

  val Venus = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(342.767053,	58519.21191,	0.0003097),
     0.7233316,
      Polynomial4(0.00682069, -0.00004774, 0.000000091),
      Polynomial4(3.393631,	0.0010058, -0.0000010),
      Polynomial4(54.384186,	0.5081861, -0.0013864),
      Polynomial4(75.779647,	0.8998500,	0.0004100)),
    new LaplacePlane(272.76,	67.16),
    6051.8
  )

  val Earth = Planet(
    new EarthPolynomialEstimator(
      Polynomial4(99.69668,    36000.76892,  0.0003025),
      1.0000002,
      Polynomial4(0.01675104, -0.0000418,   -0.000000126),
      0.0,
      Polynomial4(358.47583,   35999.04975, -0.000150, -0.0000033)),
    new LaplacePlane(0.0, 90.0),
    6378.0
  )

  val Mars = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(293.737334, 19141.69551,  0.0003107),
      1.5236883,
      Polynomial4(0.09331290, 0.000092064, -0.000000077),
      Polynomial4(1.850333,  -0.0006750,    0.0000126),
      Polynomial4(285.431761, 1.0697667,    0.0001313,  0.00000414),
      Polynomial4(48.786442,  0.7709917,    0.0000014, -0.000005330)),
    new LaplacePlane(317.67,	52.88),
    3389.5
  )

  val Jupiter = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(238.049257, 3036.301986,	0.0003347, -0.00000165),
      5.202561,
      Polynomial4(0.04833475, 0.000164180, 	-0.0000004676, -0.0000000017),
      Polynomial4(1.308736, -0.0056961, 0.0000039),
      Polynomial4(273.277558, 0.5594317, 0.00070405, 0.00000508),
      Polynomial4(99.443414, 1.0105300, 0.00035222, -0.00000851)),
    new LaplacePlane(268.06,	64.50),
    69911.0
  )

  val Saturn = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(266.564377,  1223.509884,  0.0003245, -0.0000058),
      9.554747,
      Polynomial4(0.05589232, -0.00034550, -0.000000728, 0.00000000074),
      Polynomial4(2.492519,   -0.0039189,  -0.00001549,  0.00000004),
      Polynomial4(338.307800,  1.0852207,   0.00097854,  0.00000992),
      Polynomial4(112.790414,  0.8731951,  -0.00015218, -0.00000531)),
    new LaplacePlane(40.59,83.54),
    58232.0
  )

  val Uranus = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(244.197470, 429.863546, 0.0003160, -0.00000060),
      19.21814,
      Polynomial4(0.0463444, -0.00002658, 0.000000077),
      Polynomial4(0.772464,   0.0006253,  0.0000395),
      Polynomial4(98.071581,  0.9857650,  0.0010745, -0.00000061),
      Polynomial4(73.477111,  0.4986678,  0.0013117)),
    new LaplacePlane(257.31,-15.18),
    25362.0
  )

  val Neptune = Planet(
    new NonEarthPolynomialEstimator(
      Polynomial4(84.457994, 219.885914, 0.0003205, -0.00000060),
      30.10957,
      Polynomial4(0.00899704, 0.000006330, -0.000000002),
      Polynomial4(1.779242,	-0.0095436, -0.0000091),
      Polynomial4(276.045975, 0.3256394, 0.00014095, 0.000004113),
      Polynomial4(130.681389,	1.0989350,	0.00024987,	-0.000004718)),
    new LaplacePlane(299.40,42.95),
    24622.0
  )

  val Planets = scala.collection.immutable.ListMap(
      "Mercury" -> Mercury,
      "Venus"   -> Venus,
      "Earth"   -> Earth,
      "Mars"    -> Mars,
      "Jupiter" -> Jupiter,
      "Saturn"  -> Saturn,
      "Uranus"  -> Uranus,
      "Neptune" -> Neptune)


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

      val longMean = longitudeMean(x)  * Conversions.DegToRad
      val ecc = eccentricity(x)
      val inc = inclination(x) * Conversions.DegToRad
      val argPeri = argPeriapsis(x) * Conversions.DegToRad
      val longAsc = longitudeAscending(x) * Conversions.DegToRad
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

      val longMean = longitudeMean(x) * Conversions.DegToRad
      val ecc = eccentricity(x)
      val meanAnom = meanAnomaly(x) * Conversions.DegToRad
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

    def apply(t: Double): Double =  a0 + a1 * t + a2 * t * t + a3 * t * t * t

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


object Moons {

  // https://ssd.jpl.nasa.gov/?sat_elem

  //  Common Table Column Headings:
  //    a	Semi-major Axis (mean value)
  //    e	Eccentricity (mean value)
  //    w	Argument of periapsis (mean value)
  //    M	Mean anomaly (mean value)
  //    i	Inclination with respect to the reference plane: ecliptic, ICRF, or local Laplace (mean value)
  //    node	Longitude of the ascending node (mean value) measured from the node of the reference plane on the ICRF equator
  //    n	Longitude rate (mean value)
  //    P	Sidereal period (mean value)
  //    Pw	Argument of periapsis precession period (mean value)
  //    Pnode	Longitude of the ascending node precession period (mean value)

  //  Headings only for elements with respect to the local Laplace plane:
  //    R.A.	Right ascension and ...
  //    Dec   Declination of the Laplace plane pole with respect to the ICRF.
  //    Tilt	The angle between the planet equator and the Laplace plane.

  // http://extras.springer.com/2009/978-3-540-88054-7/16_vi4b_422.pdf

  case class Moon(
    primary: OrbitalElementsEstimator,
    moon: MoonICRFEstimator,
    laplacePlane: Option[LaplacePlane],
    radiusKm: Double
  )

  // Epoch 2000 Jan. 1.50 TT
  val J_2000_01_01_12 = 2451545.0

  // Epoch 1950 Jan. 1.0 TT
  val J_1950_01_01_00 = 2433282.5

  val Luna = Moon(
    MeeusPlanets.Earth.planet,
    new MoonICRFEstimator(
      J_2000_01_01_12,
      384400.0,	0.0554,	318.15,	135.27,	5.16, 125.08, 13.176358, 27.322, 5.997, 18.600),
    None,
    1737.4
  )

  val Phobos = Moon(
    MeeusPlanets.Mars.planet,
    new MoonICRFEstimator(
      J_1950_01_01_00,
      9376.0,	0.0151,	150.057,	91.059,	1.075, 207.784, 1128.8447569, 0.3189, 1.1316, 2.2617),
    Some(new LaplacePlane(317.671, 52.893)),
    11.2667
  )

  val Deimos = Moon(
    MeeusPlanets.Mars.planet,
    new MoonICRFEstimator(
      J_1950_01_01_00,
      23458.0, 0.0002, 260.729, 325.329, 1.788, 24.525, 285.1618790,	1.2624,	27.3703,	54.5367),
    Some(new LaplacePlane(316.657, 53.529)),
    6.2
  )

  val Tethys = Moon(
    MeeusPlanets.Saturn.planet,
    new MoonICRFEstimator(
      J_2000_01_01_12,
      294672.0,	0.0001, 45.202,	243.367, 1.091,259.842,	190.6979109,	1.888,	2.490,	4.982),
    Some(new LaplacePlane(40.578, 83.537)),
    531.0
  )

  val Moons = scala.collection.immutable.ListMap(
    "Luna"   -> Luna,
    "Phobos" -> Phobos,
    "Deimos" -> Deimos,
    "Tethys" -> Tethys
  )

  class MoonICRFEstimator(
      epoch: Double,
      a: Double,
      e: Double,
      w: Double,
      m: Double,
      i: Double,
      node: Double,
      n: Double,
      val p: Double,
      val pw: Double,
      val pnode: Double) extends OrbitalElementsEstimator {

    def apply(t: Double): OrbitalElements = {

      val timeDiff = (t - epoch)

      // TODO: double check correct implementation of epoch
      val meanAnomalyDelta = timeDiff * n

      // TODO: precession of argument of periapsis
      // TODO: precession of longitude of ascending node
      val yearsSinceEpoch = timeDiff / Conversions.YearToDay

      val periodsPw = yearsSinceEpoch / pw
      val periodsPnode = yearsSinceEpoch / pnode

      val radiansPw = periodsPw * 2.0 * math.Pi
      val radiansPnode = periodsPnode * 2.0 * math.Pi

      val semiMajorAxis = a * 1000.0 / Conversions.AuToMeters

      OrbitalElements(
        longitudeMean = (node + w + m) * Conversions.DegToRad,
        semimajorAxis = semiMajorAxis,
        eccentricity = e,
        inclination = i * Conversions.DegToRad,
        argPeriapsis = w * Conversions.DegToRad + radiansPw,
        longitudeAscending = node * Conversions.DegToRad + radiansPnode,
        longitudePeriapsis = w * Conversions.DegToRad, // TODO: this is wrong!
        meanAnomaly = (m + meanAnomalyDelta) * Conversions.DegToRad,
        rp = semiMajorAxis * (1 - e),
        ra = semiMajorAxis * (1 + e)
      )
    }

  }

}