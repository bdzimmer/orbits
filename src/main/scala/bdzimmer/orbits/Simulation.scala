// Copyright (c) 2020 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import scala.collection.mutable.{Buffer => MutableBuffer}

import java.awt.{Color, Dimension}
import javax.swing.{JFrame, JSlider, JComboBox, SwingConstants}
import javax.swing.event.{ChangeEvent, ChangeListener}


object Simulation {

  val G_MKgSec = 6.67430e-11d
  val G_AuKgDay: Double = (
      G_MKgSec
      / math.pow(Conversions.AuToMeters, 3.0)
      * Conversions.DayToSec * Conversions.DayToSec)

  val SunKg = 1.989e30d

  val SunPosition = Vec3(0.0, 0.0, 0.0)
  val SunVelocity = Vec3(0.0, 0.0, 0.0)


  def main(argv: Array[String]): Unit = {

    println("start")

    val ship = ConstAccelCraft("Test", 12000.0, 0.45)

    val startDate = CalendarDateTime(2018, 5, 7)
    val endDate = CalendarDateTime(2018, 5, 21)

    val startPlanet = MeeusPlanets.Earth.planet
    val endPlanet = MeeusPlanets.Mars.planet

    // Alright! Let's calculate some forces!

    val earthKg = 5.972e24d
    val marsKg = 6.39e23d
    val lunaKg = 7.35e22d

    val lunaStateFunc = Orbits.buildMoonState(Moons.Luna)

    val bodyPositionsAuAndMassesKg: List[(String, Double => Vec3, Double)] = List(
      ("Earth", t => Orbits.planetState(MeeusPlanets.Earth.planet, t).position, earthKg),
      ("Mars", t => Orbits.planetState(MeeusPlanets.Mars.planet, t).position, marsKg),
      ("Luna", t => lunaStateFunc(t).position, lunaKg),
      ("Sun", _ => SunPosition, SunKg)
    )

    // set up starting position and velocity for an orbit of the Earth
    val radius = 0.0009
    val velocityScalar = orbitalVelocity(earthKg, radius)
    val earthPos = Orbits.planetState(MeeusPlanets.Earth.planet, startDate.julian).position
    val earthVel = Orbits.planetState(MeeusPlanets.Earth.planet, startDate.julian).velocity

    val startPosition = Vec3.add(earthPos, Vec3(radius, 0.0, 0.0))
    val startVelocity = Vec3.add(earthVel, Vec3(0.0, velocityScalar, 0.0))

    println(radius * Conversions.AuToMeters / 1000)
    println(velocityScalar * Conversions.AuToMeters / 1000 / Conversions.DayToSec)
    println(earthVel)
    println(startVelocity)

    // now update this over a bunch of time steps
    // and we'll interpolate between when sampling the flight's state function

    val states: MutableBuffer[(Double, OrbitalState)] = MutableBuffer()

    // we'll calculate once every minute
    val tickSeconds = 60.0d
    val tickInterval = tickSeconds / Conversions.DayToSec
    val ticks = (startDate.julian to endDate.julian by tickInterval)

    var curPosition = startPosition
    var curVelocity = startVelocity

    ticks.foreach(t => {

      println(t + " " + curPosition + " " + curVelocity)

      val curState = (t, OrbitalState(curPosition, curVelocity))
      states += curState

      // calculate acceleration due to gravity in AU / sec^2
      val accelGrav = gravitationalAcceleration(t, curPosition, bodyPositionsAuAndMassesKg)
      val accelEngine = Vec3(0.0, 0.0, 0.0)
      val accelTotal = Vec3.add(
        accelEngine,
        accelGrav)

      // update velocity
      curVelocity = Vec3.add(
        curVelocity,
        Vec3.mul(accelTotal, tickInterval))

      // update position
      curPosition = Vec3.add(
        curPosition,
        Vec3.mul(curVelocity, tickInterval))

    })

    println(states.length)
    val statesFiltered = states.grouped(100).map(_.head).toList

    /// ///

    val flight = PreCalculatedFlightParams(
      ship,
      "Earth Orbit",
      "Earth Orbit",
      t => Orbits.planetState(startPlanet, t),
      t => Orbits.planetState(endPlanet, t),
      startDate,
      endDate,
      List(),
      "Default",
      "Nothing",
      statesFiltered
    )

    println(flight.path.length)

    val flights: MutableBuffer[FlightParams] = MutableBuffer(flight)

    // OK, next step is a new flight type created from a list of states, I think!
    // FlightParams will have subtypes, including a "custom" type with basically a trivial
    // paramsToFun conversion. And paramsToFun will now subtypes.

    // set up interactive viewer for display

    val showSettings: ShowSettings = Editor.ShowSettingsDefault.copy(flightStatus = 0)
    val planets = showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList
    val viewerSettings = Style.ViewerSettingsDefault

    // make a little JFrame with some controls
    val controlsWindow = new JFrame("Simulation Controls")
    val slider = new JSlider(SwingConstants.HORIZONTAL, 0, states.length - 1, 0)

    def getCurDateJulian(): Double = {
      // (startDate.julian + endDate.julian) * 0.5
      val idx = slider.getValue()
      states(idx)._1
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

    // hack hack hack
    val cameraPointType = iv.cameraToolbar.getComponentAtIndex(1).asInstanceOf[JComboBox[String]]
    cameraPointType.setSelectedItem("Earth")
    iv.updateCameraControls.setZViewPos(950000)
    iv.redraw()

    // wire up slider and make controls window visible
    slider.addChangeListener(new ChangeListener() {
      override def stateChanged(changeEvent: ChangeEvent): Unit = {
        iv.redraw()
      }
    })
    controlsWindow.add(slider)
    controlsWindow.setSize(new Dimension(480, 64))
    controlsWindow.toFront()
    controlsWindow.setVisible(true)

  }

  // calculate orbital velocity in AU / day
  def orbitalVelocity(
    massKg: Double,
    axisAu: Double
  ): Double = {
    math.sqrt(G_AuKgDay * massKg / axisAu)
  }


  // calculate gravitational acceleration on a ship at a certain point in time
  // in AU / day^2
  def gravitationalAcceleration(
      curDateJulian: Double,
      shipPositionAu: Vec3,
      bodyPositionsAuAndMassesKg: List[(String, Double => Vec3, Double)]): Vec3 = {

    val dayToSec2 = Conversions.DayToSec * Conversions.DayToSec
    var total = Vec3(0.0, 0.0, 0.0)

    bodyPositionsAuAndMassesKg.foreach({case (name, posFunc, massKg) => {
      val pos = posFunc(curDateJulian)
      val dir = Vec3.sub(shipPositionAu, pos)
      val r = Vec3.length(dir)
      val accel = G_AuKgDay * massKg / (r * r)
      val accelVec = Vec3.mul(dir, -accel / r)

      println("\t" + name + " " + Vec3.length(accelVec))

      total = Vec3.add(total, accelVec)
    }})

    total

  }


}
