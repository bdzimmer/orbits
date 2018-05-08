// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import org.scalatest.FunSuite

class UnitTestsSuite extends FunSuite {

  test("solve start and end date") {

    val ship = Spacecraft("Test", 12000.0, 0.45)

    val startDate = CalendarDateTime(2018, 5, 7)
    val endDate = CalendarDateTime(2018, 5, 12)

    val startPlanet = MeeusPlanets.Earth
    val endPlanet = MeeusPlanets.Mars

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

}
