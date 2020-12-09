// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Objects that represent flight paths and the various information
// to be tracked along with them.

package bdzimmer.orbits

import scala.collection.immutable.Seq

// This is sort of a parallel version of how flights
// are represented in Secondary

trait FlightParams {

    val ship: Spacecraft
    val origName: String  // might degeneralize some of this eventually
    val destName: String
    val orig: Double => OrbitalState
    val dest: Double => OrbitalState
    val startDate: CalendarDateTime
    val endDate: CalendarDateTime
    val passengers: List[String]
    val faction: String
    val description: String

  override def toString: String = {
    startDate.dateString + " - " +
    ship.name.replace("*", "") + " - " +
    origName + " -> " + destName
  }

}


// Basic FlightParams we've had up until this point
// Either constant acceleration or constant velocity depending on ship type
case class SimpleFlightParams(
    ship: Spacecraft,
    origName: String,
    destName: String,
    orig: Double => OrbitalState,
    dest: Double => OrbitalState,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    passengers: List[String],
    faction: String,
    description: String
) extends FlightParams


// FlightParams based on a precalculated path
case class PreCalculatedFlightParams(
    ship: Spacecraft,
    origName: String,
    destName: String,
    orig: Double => OrbitalState,
    dest: Double => OrbitalState,
    startDate: CalendarDateTime,
    endDate: CalendarDateTime,
    passengers: List[String],
    faction: String,
    description: String,
    path: List[(Double, OrbitalState)]  // precalculated path
) extends FlightParams



object FlightParams {

  def paramsToFun(fp: FlightParams): (FlightFn, scala.collection.immutable.Seq[Double]) = {

    val startDateJulian = fp.startDate.julian
    val endDateJulian = fp.endDate.julian

    fp match {
      case sfp: SimpleFlightParams => {
        val res = if ((endDateJulian - startDateJulian) > 1.0) {
          // one tick per hour
          1.0 / 24.0
        } else {
          // one tick per minute
          1.0 / 24.0 / 60.0
        }

        // don't ask
        val ticks = (startDateJulian until endDateJulian by res).toList.toIndexedSeq

        // find positions of origin and destination bodies
        val origState = sfp.orig(startDateJulian)
        val destState = sfp.dest(endDateJulian)

        val flightFn = sfp.ship match {
          case _: ConstAccelCraft => ConstAccelFlightFn(
            origState.position, destState.position,
            startDateJulian, endDateJulian - startDateJulian)
          case _: ConstVelCraft => ConstVelFlightFn(
            origState.position, destState.position,
            startDateJulian, endDateJulian - startDateJulian
          )
        }

        (flightFn, ticks)

      }
      case pcfp: PreCalculatedFlightParams => {
        val ticks = pcfp.path.map(_._1)
        val flightFn = new LinearInterpFlightFn(pcfp.path)
        (flightFn, ticks)
      }
      case _ => (new DummyFlightFn, Seq(0.0d))

    }


  }


}
