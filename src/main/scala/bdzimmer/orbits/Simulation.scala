// Copyright (c) 2020 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import scala.collection.mutable.{Buffer => MutableBuffer}
import scala.collection.immutable.Seq

import java.io.File
import java.awt.{Color, Dimension, BorderLayout}
import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage

import javax.swing.{JButton, JComboBox, JFrame, JSlider, SwingConstants}
import javax.swing.event.{ChangeEvent, ChangeListener}
import bdzimmer.util.StringUtils._
import org.apache.commons.imaging.{ImageFormats, Imaging}


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
    val endDate = CalendarDateTime(2018, 5, 9)

    val startDateJulian = startDate.julian

    val startPlanet = MeeusPlanets.Earth.planet
    val endPlanet = MeeusPlanets.Mars.planet

    // Alright! Let's calculate some forces!

    val earthKg = 5.972e24d
    val marsKg = 6.39e23d
    val lunaKg = 7.35e22d

    val lunaStateFunc = Orbits.buildMoonState(Moons.Luna)
    val earthStateFunc = {t: Double => Orbits.planetState(MeeusPlanets.Earth.planet, t)}
    val lunaStateRelFunc = {t: Double => {
        val stateEarth = earthStateFunc(t)
        val stateMoon = lunaStateFunc(t)
        OrbitalState(
          Vec3.sub(stateMoon.position, stateEarth.position),
          Vec3.sub(stateMoon.velocity, stateEarth.velocity))
    }}

    val bodyPositionsAuAndMassesKg: List[(String, Double => Vec3, Double)] = List(
      ("Earth", t => earthStateFunc(t).position, earthKg),
      ("Mars", t => Orbits.planetState(MeeusPlanets.Mars.planet, t).position, marsKg),
      ("Luna", t => lunaStateFunc(t).position, lunaKg),
      ("Sun", _ => SunPosition, SunKg)
    )

    val earthOrbitRadius = 0.0009
    val moonOrbitRadius = 0.0002

//    // set up starting position and velocity for an orbit of the Earth
//    val velocityScalar = orbitalVelocity(earthKg, earthOrbitRadius)
//    val earthPos = earthStateFunc(startDate.julian).position
//    val earthVel = earthStateFunc(startDate.julian).velocity
//
//    val startPosition = Vec3.add(earthPos, Vec3(earthOrbitRadius, 0.0, 0.0))
//    val startVelocity = Vec3.add(earthVel, Vec3(0.0, velocityScalar, 0.0))
//
//    println(earthOrbitRadius * Conversions.AuToMeters * Conversions.MetersToKm)
//    println(
//      velocityScalar
//        * Conversions.AuToMeters
//        * Conversions.MetersToKm
//        * Conversions.DayToSec)
//
//    println(earthVel)
//    println(startVelocity)
//
//    (startPosition, startVelocity)

    // trying to plan a real flight from orbit of the Earth to the Moon

    // plan a constant acceleration flight from the Earth to the Moon
    // ignore movement of Earth and Moon around the Sun
    // for accuracy here

    val flightEndDateJulian: Double = SolveFlight.constAccelEndDate(
      ship,
      Transformations.Vec3Zero,
      t => lunaStateRelFunc(t).position,
      startDate.julian).getOrElse(startDate.julian)

    val flightEndDate = Conversions.julianToCalendarDate(flightEndDateJulian)

    print(startDate.julian, flightEndDate.julian)

    val flightEst = SimpleFlightParams(
      ship,
      "Earth",
      "Luna",
      earthStateFunc,
      lunaStateFunc,
      startDate,
      flightEndDate,
      List(),
      "Default",
      "Nothing")

    // now that we have an estimate of the time, find the outer tangent between
    // Earth at the start time and the Moon at the end time. This will give us our
    // true starting and target ending positions.

    val earthStartPos = earthStateFunc(startDate.julian).position

    val (startPosition, endPosition) = outerTangentEstimated3D(
      earthStartPos,
      earthOrbitRadius,
      Vec3.add(
        earthStartPos,
        lunaStateRelFunc(flightEndDateJulian).position),
      moonOrbitRadius,
      false)

    // calculate starting velocity...orbital velocity at starting position
    // perpendicular to tangent vector
    val startVelocityScalar = 1.0 * orbitalVelocity(earthKg, earthOrbitRadius)
//    val startVelocityDirection = Vec3.cross(
//        Vec3.sub(earthStartPos, startPosition),
//        Transformations.UnitZ)
    val startVelocityDirection = Vec3.sub(endPosition, startPosition)
    val svu = Vec3.mul(
      startVelocityDirection,
      1.0 / Vec3.length(startVelocityDirection))
    val startVelocity = Vec3.add(
      earthStateFunc(startDate.julian).velocity,
      Vec3.mul(svu, startVelocityScalar))

    // define an engine acceleration function:
    //    - accelerate in starting velocity direction for first half of trip
    //    - accelerate in opposite direction for second half of trip
    val svua = Vec3.emul(svu, Vec3(1.0, 1.0, 1.0))
    // val tp = Vec3(0.47, 0.47, 0.47)
    val tp = Vec3(0.5, 0.5, 0.5)

    def accelEngineFn(t: Double): Vec3 = {
      val tNorm = (t - startDateJulian) / (flightEndDateJulian - startDateJulian)

      val x = if (tNorm >= 0.0 && tNorm < tp.x) {
        svua.x * ship.accel
      } else if (tNorm >= tp.x && tNorm <= 1.0) {
        svua.x * -ship.accel
      } else {
        0.0
      }

      val y = if (tNorm >= 0.0 && tNorm < tp.y) {
        svua.y * ship.accel
      } else if (tNorm >= tp.y && tNorm <= 1.0) {
        svua.y * -ship.accel
      } else {
        0.0
      }

      val z = if (tNorm >= 0.0 && tNorm < tp.z) {
        svua.z * ship.accel
      } else if (tNorm >= tp.z && tNorm <= 1.0) {
        svua.z * -ship.accel
      } else {
        0.0
      }

      Vec3(x, y, z)

    }

    // now update this over a bunch of time steps
    // and we'll interpolate between when sampling the flight's state function

    val states: MutableBuffer[(Double, OrbitalState)] = MutableBuffer()

    // we'll calculate once every minute
    val tickSeconds = 60.0d
    val tickInterval = tickSeconds / Conversions.DayToSec
    val ticks = (startDate.julian until endDate.julian by tickInterval)

    var curPosition = startPosition
    var curVelocity = startVelocity

    ticks.foreach(t => {

      // println(t + " " + curPosition + " " + curVelocity)

      val curState = (t, OrbitalState(curPosition, curVelocity))
      states += curState

      // calculate acceleration due to gravity in AU / sec^2
      val accelGrav = gravitationalAcceleration(t, curPosition, bodyPositionsAuAndMassesKg)
      // val accelEngine = Vec3(0.0, 0.0, 0.0)

      val accelEngine = accelEngineFn(t)

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
    val statesFiltered = states.grouped(5).map(_.head).toList
    println(statesFiltered.length)

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

    // ~~~~ evaluate flight function ~~~~

    val pcFlightFunc = FlightParams.paramsToFun(flight)._1
    val endState = ConvertFlight.flightState(pcFlightFunc, flightEndDateJulian)

    // println(s"position ${Vec3.sub(endState.position, endPosition)}  (actual - target)")

    // desired velocity is orbital velocity of moon (in outer tangent direction)
    // plus velocity of moon
    // val rv = Vec3.sub(curPosition, endState.position)
    // val rvn = Vec3.mul(rv, 1.0 / Vec3.length(rv))

    val rv = Vec3.cross(
      Vec3.sub(endState.position, lunaStateFunc(flightEndDateJulian).position),
      Transformations.UnitZ)
    val radius = Vec3.length(rv)
    val rvn = Vec3.mul(rv, 1.0 / Vec3.length(rv))

    val desiredVelocity = orbitalVelocity(lunaKg, radius)
    val dvVec = Vec3.add(
      Vec3.mul(rvn, desiredVelocity),
      lunaStateFunc(flightEndDateJulian).velocity)

    println(s"radius: target: ${moonOrbitRadius}  actual: ${radius}")
    println(s"svu: ${svu}")
    println(s"velocity  target: ${Vec3.length(dvVec)}  actual: ${Vec3.length(endState.velocity)}")
    println(s"\t${Vec3.sub(endState.velocity, dvVec)}  (actual - target)")

    // val desiredOrbitalRadius = moonOrbitRadius
//    val radius = Vec3.length(
//      Vec3.sub(curPosition, endState.position))
//    println(s"radius    target: ${desiredOrbitalRadius}  actual: ${radius}")

    // ~~~~ ~~~~ ~~~~ ~~~~

    // set up flights and measurements

    val flights: MutableBuffer[FlightParams] = MutableBuffer(flightEst, flight)

    val viewerSettings = Style.ViewerSettingsDefault

    val measurements: Seq[Measurement] = Seq(
      MeasurementFuncDistance(
        "initial outer tangent",
        t => {
          val earthPos = earthStateFunc(t).position
//          (
//            Vec3.add(Vec3.sub(startPosition, earthStartPos), earthPos),
//            Vec3.add(Vec3.sub(endPosition, earthStartPos), earthPos)
//          )
          (startPosition, endPosition)
        },
        (0, viewerSettings.lineHeightSmall * 2)
      ),
      MeasurementFuncDistance(
        "Outer tangent",
        t => {
          outerTangentEstimated3D(
            earthStateFunc(t).position, earthOrbitRadius,
            lunaStateFunc(t).position, moonOrbitRadius,
            false)
        },
        (0, viewerSettings.lineHeightSmall * 2)
      ),
      MeasurementLookupDistance("Flight", "Earth", (0, viewerSettings.lineHeightSmall * 2)),
      MeasurementLookupRelativeVelocity("Flight", "Earth", (0, viewerSettings.lineHeightSmall * 5)),
      MeasurementLookupDistance("Flight", "Luna", (0, viewerSettings.lineHeightSmall * 2)),
      MeasurementLookupRelativeVelocity("Flight", "Luna", (0, viewerSettings.lineHeightSmall * 5))
    )

    // OK, next step is a new flight type created from a list of states, I think!
    // FlightParams will have subtypes, including a "custom" type with basically a trivial
    // paramsToFun conversion. And paramsToFun will now subtypes.

    // set up interactive viewer for display

    val showSettings: ShowSettings = Editor.ShowSettingsDefault.copy(flightStatus = 0)
    val planets = showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList

    // make a little JFrame with some controls
    val controlsWindow = new JFrame("Simulation Controls")
    val slider = new JSlider(SwingConstants.HORIZONTAL, 0, statesFiltered.length - 1, 0)

    def getCurDateJulian(): Double = {
      val idx = slider.getValue()
      val endDateJulian = endDate.julian
      val diff = endDateJulian - startDateJulian
      val frac = idx / slider.getMaximum.toDouble
      val res = startDateJulian + frac * diff
      // println(s"$startDateJulian $res $endDateJulian")
      res
    }

    // val flights: scala.collection.mutable.Buffer[FlightParams] = scala.collection.mutable.Buffer()
    val factions = Map(("Default", Color.GREEN))

    val iv = new InteractiveView(
      "Orbits Edtior",
      getCurDateJulian,
      () => true,  // timeline mode will be false once we have a flight
      () => planets,
      () => flights,
      () => flight,
      () => measurements,
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

    // wire up slider and make controls window visible
    slider.addChangeListener(new ChangeListener() {
      override def stateChanged(changeEvent: ChangeEvent): Unit = {
        iv.redraw()
      }
    })
    slider.setSize(new Dimension(480, 64))
    controlsWindow.add(slider, BorderLayout.NORTH)

    val exportButton = new JButton("Export")
    exportButton.addActionListener(new ActionListener() {
      override def actionPerformed(actionEvent: ActionEvent): Unit = {
        // putting this in a thread allows the UI to update so we can see progress
        val thread = new Thread(new Runnable() {
          override def run(): Unit = {
            exportAnimation(iv, slider)
          }
        })
        thread.start()
      }
    })
    controlsWindow.add(exportButton, BorderLayout.SOUTH)

    controlsWindow.pack()
    controlsWindow.toFront()
    controlsWindow.setVisible(true)

    iv.redraw()

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

    var total = Vec3(0.0, 0.0, 0.0)

    bodyPositionsAuAndMassesKg.foreach({case (name, posFunc, massKg) => {
      val pos = posFunc(curDateJulian)
      val dir = Vec3.sub(shipPositionAu, pos)
      val r = Vec3.length(dir)
      val accel = G_AuKgDay * massKg / (r * r)
      val accelVec = Vec3.mul(dir, -accel / r)

      // println("\t" + name + " " + Vec3.length(accelVec))

      total = Vec3.add(total, accelVec)
    }})

    total

  }


  // find outer tangent between two circles
  def outerTangent2D(
      p1: Vec2, r1: Double,
      p2: Vec2, r2: Double,
      plusOrMinus: Boolean): (Vec2, Vec2) = {

    val factor = if (plusOrMinus) 1.0 else - 1.0
    val p1p2 = Vec2.sub(p2, p1)

    val gamma = -math.atan2(p1p2.y, p1p2.x)
    val beta = factor * math.asin((r2 - r1) / Vec2.length(p1p2))

    val alpha = gamma - beta

    val p3 = Vec2(
      p1.x + factor * r1 * math.sin(alpha),
      p1.y + factor * r1 * math.cos(alpha))

    val p4 = Vec2(
      p2.x + factor * r2 * math.sin(alpha),
      p2.y + factor * r2 * math.cos(alpha))

    (p3, p4)
  }


  def outerTangentEstimated3D(
      p1: Vec3,
      r1: Double,
      p2: Vec3,
      r2: Double,
      plusOrMinus: Boolean): (Vec3, Vec3) = {

    val (tan1, tan2) = outerTangent2D(
      Vec2(p1.x, p1.y), r1,
      Vec2(p2.x, p2.y), r2,
      plusOrMinus)

    (
      Vec3(tan1.x, tan1.y, p1.z),
      Vec3(tan2.x, tan2.y, p2.z)
    )
  }


  def exportAnimation(iv: InteractiveView, slider: JSlider): String = {
    val writeTimeStart = System.currentTimeMillis

    val width = 1280
    val height = 720

    val datetimeString = InteractiveView.currentDatetimeString()
    val outputDirname = "simulation_" + datetimeString
    new File(outputDirname).mkdirs()

    (slider.getMinimum to slider.getMaximum).foreach(idx => {
      slider.setValue(idx)
      val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      iv.redrawGeneric(image)
      val frameFile = new File(outputDirname / f"$idx%05d.png")
      Imaging.writeImage(
        image, frameFile, ImageFormats.PNG, new java.util.HashMap[String, Object]())
    })

    RenderFlight.imagesToVideo(
      outputDirname,
      outputDirname / "animation.mp4",
      width,
      height,
      30)

    val writeTime = System.currentTimeMillis - writeTimeStart

    print("exported " + outputDirname + " in " + writeTime / 1000.0 + " sec")

    outputDirname
  }

}
