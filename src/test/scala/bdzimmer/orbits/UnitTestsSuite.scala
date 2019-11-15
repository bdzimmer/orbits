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

}
