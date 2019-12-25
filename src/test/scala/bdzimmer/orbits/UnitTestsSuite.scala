// Copyright (c) 2018 Ben Zimmer. All rights reserved.

// Various unit tests.

package bdzimmer.orbits

import org.scalatest.FunSuite


class UnitTestsSuite extends FunSuite {

  test("solve start and end date") {

    val ship = ConstAccelCraft("Test", 12000.0, 0.45)

    val startDate = CalendarDateTime(2018, 5, 7)
    val endDate = CalendarDateTime(2018, 5, 12)

    val startPlanet = MeeusPlanets.Earth.planet
    val endPlanet = MeeusPlanets.Mars.planet

    // solve in one guessDelta

    assert(
      SolveFlight.endDate(
        ship,
        Orbits.planetState(startPlanet, startDate.julian).position,
        t => Orbits.planetState(endPlanet, t).position,
        startDate.julian
      ).isDefined)

    assert(
      SolveFlight.startDate(
        ship,
        t => Orbits.planetState(startPlanet, t).position,
        Orbits.planetState(endPlanet, endDate.julian).position,
        endDate.julian
      ).isDefined)

    // can't solve in one guessDelta

    assert(
      SolveFlight.endDate(
        ship,
        Orbits.planetState(startPlanet, startDate.julian).position,
        t => Orbits.planetState(endPlanet, t).position,
        startDate.julian, 1.0
      ).isEmpty)

    assert(
      SolveFlight.startDate(
        ship,
        t => Orbits.planetState(startPlanet, t).position,
        Orbits.planetState(endPlanet, endDate.julian).position,
        endDate.julian, 1.0
      ).isEmpty)

  }

  test("convert between calendar date time and julian date") {
    val startDate = CalendarDateTime(2690, 9, 1, 12, 30, 30.0).julian
    for (date <- startDate until startDate + 10.0 by 0.11) {
      // convert julian date to CalendarDateTime and back to julian date
      val cdt = Conversions.julianToCalendarDate(date)
      val julian = cdt.julian
      val diff = julian - date
      println(date + "\t" + cdt.dateTimeString + "\t" + julian + "\t" + diff)
      assert(math.abs(diff) < 1.0e-8)
    }
  }

  test("check tilts and inclinations") {
    val date = CalendarDateTime(2010, 6, 14, 12, 0, 0.0).julian
    val planetName = "Mars"
    val planet = MeeusPlanets.Planets(planetName)

    val earth = MeeusPlanets.Earth

    def angleDegrees(a: Vec3, b: Vec3): Double = {
     val res = math.acos(Vec3.dot(a, b)) / Conversions.DegToRad
     math.rint(res * 1000.0) / 1000.0
    }

    val zAxis = Transformations.UnitZ

    val oe = planet.planet(date)

    val orbitalToIntertial = Orbits.transformOrbitalInertial(oe)

    val axisOrbital = orbitalToIntertial.mul(zAxis)

    val earthAxis = Orbits.laplacePlaneICRFTransformation(
      earth.axialTilt.rightAscension, earth.axialTilt.declination)
    val planetAxis = Orbits.laplacePlaneICRFTransformation(
      planet.axialTilt.rightAscension, planet.axialTilt.declination).mul(zAxis)

    println()
    println(planetName)
    println("orbital inc to ecliptic: " + angleDegrees(zAxis, axisOrbital))
    println("axial tilt to orbit:     " + angleDegrees(planetAxis, axisOrbital))
    println("axial tilt to ecliptic:  " + angleDegrees(planetAxis, zAxis))


  }


  test("orbital elements of mercury") {

    // Mercury's orbital elements are evaluated at this date
    // as an example in *Astronomical Formulae for Calculators*

    val date = 2443683.5
    val elements = MeeusPlanets.Mercury.planet(date)

    println(elements.longitudeMean / Conversions.DegToRad)
    println(elements.semimajorAxis)
    println(elements.eccentricity)
    println(elements.inclination / Conversions.DegToRad)
    println(elements.argPeriapsis / Conversions.DegToRad)
    println(elements.longitudeAscending / Conversions.DegToRad)

  }

}
