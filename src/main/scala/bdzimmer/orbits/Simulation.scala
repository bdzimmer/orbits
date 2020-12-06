package bdzimmer.orbits

import scala.collection.mutable.Buffer
import java.awt.Color

import scala.collection.mutable


object Simulation {

  val G = 6.67430e-11d
  val GAuKgSec: Double = G / (math.pow(Conversions.AuToMeters, 3.0))

  val SunKg = 1.989e30d

  val SunPosition = Vec3(0.0, 0.0, 0.0)
  val SunVelocity = Vec3(0.0, 0.0, 0.0)


  def main(argv: Array[String]): Unit = {
    // println("G:" + G)
    // println("GAuKgSec: " + GAuKgSec)
    println("start")

    val ship = ConstAccelCraft("Test", 12000.0, 0.45)

    val startDate = CalendarDateTime(2018, 5, 7)
    val endDate = CalendarDateTime(2018, 5, 12)

    val startPlanet = MeeusPlanets.Earth.planet
    val endPlanet = MeeusPlanets.Mars.planet

//    val result = SolveFlight.endDate(
//      ship,
//      Orbits.planetState(startPlanet, startDate.julian).position,
//      t => Orbits.planetState(endPlanet, t).position,
//      startDate.julian)

    // Alright! Let's calculate some forces!

    val earthKg = 5.972e24d
    val marsKg = 6.39e23d
    val lunaKg = 7.35e22d

    val lunaStateFunc = Orbits.buildMoonState(Moons.Luna)

    val bodyPositionsAuAndMassesKg: List[(Double => Vec3, Double)] = List(
      (t => Orbits.planetState(MeeusPlanets.Earth.planet, t).position, earthKg),
      (t => Orbits.planetState(MeeusPlanets.Mars.planet, t).position, marsKg),
      (t => lunaStateFunc(t).position, lunaKg),
      (_ => SunPosition, SunKg)
    )

    // starting position of ship halfway between Earth and Moon
    val startPosition = // Vec3.mul(
      Vec3.add(
        Orbits.planetState(MeeusPlanets.Earth.planet, startDate.julian).position,
        lunaStateFunc(startDate.julian).position)
      // 0.5d)

    // starting velocity will be half the velocity of the moon at that time
    val startVelocity = Vec3.mul(
      lunaStateFunc(startDate.julian).velocity,
      0.5d)

    // now update this over a bunch of time steps
    // and we'll interpolate between when sampling the flight's state function

    val states: mutable.Buffer[(Double, OrbitalState)] = mutable.Buffer()

    // we'll calculate this for 24 hours
    // once every minute
    val tickSeconds = 60.0d
    val tickInterval = tickSeconds * 1.0 / Conversions.DayToSec
    val ticks = (startDate.julian to (startDate.julian + 0.1) by tickInterval)

    var curPosition = startPosition
    var curVelocity = startVelocity

    ticks.foreach(t => {

      println(t + " " + curPosition + " " + curVelocity)

      val curState = (t, OrbitalState(curPosition, curVelocity))
      states += curState

      // calculate acceleration due to gravity in AU / sec^2
      val accelGrav = gravity(t, curPosition, bodyPositionsAuAndMassesKg)
      // TODO: engine acceleration
      val accelTotal = accelGrav  // for now

      println(accelTotal)

      // update velocity
      curVelocity = Vec3.add(
        curVelocity,
        Vec3.mul(accelTotal, tickInterval))

      // update position
      curPosition = Vec3.add(
        curPosition,
        Vec3.mul(curVelocity, tickInterval))

    })


    /// ///

    val flight = FlightParams(
      ship,
      "Earth",
      "Mars",
      t => Orbits.planetState(startPlanet, t),
      t => Orbits.planetState(endPlanet, t),
      startDate,
      endDate,
      List(),
      "Default",
      "Nothing")

    val flights = scala.collection.mutable.Buffer(flight)

    // OK, next step is a new flight type created from a list of states, I think!
    // FlightParams will have subtypes, including a "custom" type with basically a trivial
    // paramsToFun conversion. And paramsToFun will now subtypes.

    // set up interactive viewer for display

    // TODO: make a little JFrame with some controls

    val showSettings = Editor.ShowSettingsDefault
    val planets = showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList
    val viewerSettings = Style.ViewerSettingsDefault

    def getCurDateJulian(): Double = {
      (startDate.julian + endDate.julian) * 0.5
    }

    // val flights: scala.collection.mutable.Buffer[FlightParams] = scala.collection.mutable.Buffer()
    val factions = Map(("Default", Color.GREEN))

    val iv = new InteractiveView(
      "Orbits Edtior",
      getCurDateJulian,
      () => true,  // timeline mode will be false once we have a flight
      () => planets,
      () => flights,
      () => flight,  // bruh
      () => factions,
      () => false,
      () => true,
      showSettings,
      viewerSettings
    )

  }


  // calculate gravitational acceleration on a ship at a certain point in time
  // in kg * AU / sec^2
  def gravity(
      curDateJulian: Double,
      shipPositionAu: Vec3,
      bodyPositionsAuAndMassesKg: List[(Double => Vec3, Double)]): Vec3 = {

    var total = Vec3(0.0, 0.0, 0.0)

    bodyPositionsAuAndMassesKg.foreach({case (posFunc, massKg) => {
      val pos = posFunc(curDateJulian)
      val dir = Vec3.sub(shipPositionAu, pos)
      val r = Vec3.length(dir)
      val accel = GAuKgSec * massKg / (r * r)
      val accelVec = Vec3.mul(dir, accel / r)
      total = Vec3.add(total, accelVec)
    }})

    total

  }


}
