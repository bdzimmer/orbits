// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Functionality for drawing multiple flights, planets, and flight paths onto an image.

package bdzimmer.orbits

import java.awt.{Color, Graphics2D}
import java.awt.image.BufferedImage

import scala.collection.immutable.Seq


object Draw {

  val GridLim = 50  // radius of solar system is about 50 AU

  // draw asteroid belt - main belt lies between 2.06 and 3.27 AU
  val BeltR0 = 2.06
  val BeltR1 = 3.27

  val MoonsExperiment = true
  val DetailMin =  12.0

  // constant colors - eventually these could become configurable
  val MeasurementColor = Color.GRAY
  val GridColor = new Color(0, 0, 128)

  val EpsVel: Double = 0.0001


  def redraw(

      fpOption: Option[FlightParams],       // flight to highlight with status
      curDateJulian: Double,

      planets: Seq[(String, Planet)],
      flights: Seq[FlightParams],
      measurements: Seq[Measurement],

      factions: Map[String, Color],

      asteroidBelt: Boolean,
      lagrangePoints: Boolean,
      orbitInfo: Boolean,
      motionVerticals: Boolean,
      statusOption: Int,
      viewerSettings: ViewerSettings,

      camInfo: (Mat33, Vec3),

      viewPos: Vec3,

      im: BufferedImage): scala.collection.mutable.Map[String, Vec2] = {

    // TODO: move this to a higher level
    // clear the image
    val gr = im.getGraphics
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, im.getWidth, im.getHeight)

    // camera prep
    val (camRot, camPos) = camInfo
    val camTrans = View.cameraTransform(camRot, camPos)

    val objects: scala.collection.mutable.Map[String, Vec2] = new scala.collection.mutable.HashMap[String, Vec2]()

    val view = new Viewer(camTrans, viewPos, viewerSettings)

    val planetMotions = planets.map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2.planet, curDateJulian))
    })

    // find other flights that are active at the same time as the current one
    val activeFlights = flights.filter(x =>
      // !x.equals(fp) &&
      x.startDate.julian <= curDateJulian && x.endDate.julian >= curDateJulian)

    // ~~~~

    // draw the grid and the sun
    view.drawGrid(im, GridLim, GridLim, None, GridColor)
    val sun = Vec3(0.0, 0.0, 0.0)
    view.drawPosition(im, sun, "Sun", "", Color.YELLOW) // for now

    // draw the orbits of planets and their positions
    // the sequence of orbital states for each planet should start from same time
    // as the final state of the flight - this is the time that we are drawing the
    // flight at
    planetMotions.foreach(x => RenderFlight.drawOrbit(im, x._2, view, Color.LIGHT_GRAY, motionVerticals))

    // draw the position of each planet
    planets.zip(planetMotions).foreach({case (x, y) => {

      // calculate viewport diameter of planet
      val pos = y._2.last.position

      // TODO: don't draw if the camera position is right on top of it
      val camToPlanet = Vec3.length(Vec3.sub(camPos, pos))
      val camToPlanetThresh = 1.0e-6

      if (camToPlanet > camToPlanetThresh) {

        val name = x._1

        val radiusAu = x._2.radiusKm * 1000.0 / Conversions.AuToMeters

        // TODO: this could be cached for the planet
        val tilt = Orbits.laplacePlaneICRFTransformation(
          x._2.axialTilt.rightAscension, x._2.axialTilt.declination)

        val rotationAngle = (x._2.rotDegPerDay * curDateJulian) % 360.0
        val rotation = Transformations.rotZ(rotationAngle * Conversions.DegToRad)
        val sphereTrans = Transformations.transformation(tilt.mul(rotation), pos)
        val (_, _, _, _, diameterView) = sphereInfo(radiusAu, sphereTrans, camTrans, viewPos)

        // if zoomed in far enough to see it, draw detailed view
        if (diameterView > DetailMin) {
          // ~~~~ detailed planet view ~~~~

          // coordinates in the viewer image
          val pos2d = View.perspective(pos, camTrans, viewPos)
          val (ptx, pty) = Viewer.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)

          if (Viewer.inView(im, ptx, pty)) {

            val scale = Vec3(radiusAu, radiusAu, radiusAu)

            view.drawSphere(
              im,
              sphereTrans,
              scale,
              Color.LIGHT_GRAY,
              true)

            val gr = im.getGraphics.asInstanceOf[Graphics2D]
            gr.setRenderingHints(Viewer.RenderHints)
            val titleColor = Color.LIGHT_GRAY

            gr.setFont(view.settings.displayFontLarge)

            // put the name of the planet near the planet
            // gr.drawString(name, ptx + rad, pty)

            // put the name of the planet dramatically near the bottom of the screen
            // TODO: this width could be cached for each planet
            val lineWidth = gr.getFontMetrics(view.settings.displayFontLarge).stringWidth(name)

            // fade the color in dramatically as a function of zoom
            // val fade = math.min(((viewPos.z - DetailMin) / (DetailMax - DetailMin)).toFloat, 1.0f)
            val detailMax = im.getHeight * 0.25
            val fade = math.min(((diameterView - DetailMin) / (detailMax - DetailMin)).toFloat, 1.0f)

            gr.setColor(new Color(
              (titleColor.getRed * fade).toInt,
              (titleColor.getGreen * fade).toInt,
              (titleColor.getBlue * fade).toInt))

            gr.drawString(
              name.toUpperCase, im.getWidth / 2 - lineWidth / 2,
              im.getHeight - view.settings.lineHeightLarge)

          }
        } else {
          view.drawPosition(im, y._2.last.position, x._1, "", Color.LIGHT_GRAY)
        }

        objects(name) = View.perspective(pos, view.camTrans, view.viewPos)

      }

    }})

    // view.drawSphere(im, Transformations.IdentityTransformation, Vec3(0.5, 0.5, 0.5), Color.GREEN)

    // ~~~~ ~~~~ Experimentation with moons ~~~~ ~~~~

    if (MoonsExperiment) {
      Moons.Moons.foreach({case (name, moon) => {

        // TODO: consider putting this in the detailed planet view above

        // TODO: remove ICRF transformation from calculation
        // TODO: this can be precalculated for each moon
        val laplacePlane = Some(
          moon.laplacePlane.map(
            y => Orbits.laplacePlaneICRFTransformation(y.rightAscension, y.declination)
          ).getOrElse(
            // Conversions.ICRFToEcliptic
            Transformations.Identity3
          ))

        // motion of the moon relative to the primary
        val motion = Orbits.moonMotionPeriod(moon.primary, moon.moon, laplacePlane, curDateJulian)

        // TODO: this should be calculated separately
        // so the full period isn't required if we aren't going to draw it anyway
        val moonPos = motion.last.position

        // find the radius of the orbit in the viewer
        val primaryPos = Orbits.planetState(moon.primary, curDateJulian).position
        val moonPosView = View.perspective(moonPos, camTrans, viewPos)
        val primaryPosView = View.perspective(primaryPos, camTrans, viewPos)
        val radiusView = Vec2.length(Vec2.sub(moonPosView, primaryPosView))

        if (radiusView > DetailMin) {
          RenderFlight.drawOrbit(im, motion, view, Color.LIGHT_GRAY, motionVerticals)

          view.drawPosition(im, moonPos, name, "", Color.LIGHT_GRAY)

          if (orbitInfo) {

            val preTrans = Transformations.transformation(
              laplacePlane.getOrElse(Transformations.Identity3),
              Orbits.planetState(moon.primary, curDateJulian).position)

            RenderFlight.drawOrbitInfo(
              im,
              moon.moon(curDateJulian),
              preTrans,
              view)

            val pMotion = RenderFlight.precessionPeriod(moon.moon, curDateJulian, preTrans)
            RenderFlight.drawOrbit(
              im, pMotion, view,
              RenderFlight.ColorOrbital, motionVerticals, true)

          }

          objects(name) = View.perspective(motion.last.position, view.camTrans, view.viewPos)

        }

      }})

    }

    // ~~~~ ~~~~ ~~~~ ~~~~

    // draw measurements
    val flightFn = fpOption.map(x => FlightParams.paramsToFun(x)._1)
    measurements.foreach(x => drawMeasurement(im, view, x, flightFn, curDateJulian))


    // draw orbit info
    if (orbitInfo) {
      planets.foreach(x => {
        val oe = x._2.planet(curDateJulian)
        RenderFlight.drawOrbitInfo(
          im, oe, Transformations.IdentityTransformation, view)
      })
    }


    // draw L3, L4 and L5 points of visible planets
    if (lagrangePoints) {
      // TODO: would be nice to not reconstruct these every time
      planets.foreach(p => {
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L3Estimator(p._2.planet), curDateJulian).position,
          "L3", "", Color.LIGHT_GRAY, fill = false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L4Estimator(p._2.planet), curDateJulian).position,
          "L4", "", Color.LIGHT_GRAY, fill = false)
        view.drawPosition(
          im, Orbits.planetState(new MeeusPlanets.L5Estimator(p._2.planet), curDateJulian).position,
          "L5", "", Color.LIGHT_GRAY, fill = false)
      })
    }

    // draw the asteroid belt
    if (asteroidBelt) {
      view.drawRing(im, BeltR0, BeltR1, new Color(64, 64, 64, 128))
    }

    // draw other flights in the background
    // TODO: optional ship arrows and names?
    // otherFlights.zip(otherFlightsColors).toList.foreach(x => view.drawMotion(im, x._1, x._2))

    activeFlights.foreach(flight => {
      val (flightFn, ticksPartial) = FlightParams.paramsToFun(flight)
      val ticks = (ticksPartial.filter(x => x <= curDateJulian).toList :+ curDateJulian)
      val positions = ticks.map(tick => flightFn(tick))
      val origStates = ticks.map(tick => flight.orig(tick))

      val factionColor = factions.getOrElse(flight.faction, Color.LIGHT_GRAY)

      val vel = flight match {
        case _: PreCalculatedFlightParams => {
          // draw absolute motion and velocity arrow
          /// view.drawMotion(im, positions, factionColor, true, verticals = motionVerticals)
          // val vel = drawVelocityArrow(im, view, curDateJulian, flightFn, factionColor)

          // velocity relative to origin
          val curState = stateFromFlightFn(curDateJulian, flightFn)
          val origCur = flight.orig(curDateJulian)
          val velVecRel = Vec3.sub(curState.velocity, origCur.velocity)
          // draw the arrow
          val vel = Vec3.length(velVecRel)
          if (vel > ConstVelFlightFn.VelMin) {
            view.drawArrow(im, OrbitalState(curState.position, velVecRel), factionColor)
          }

          // draw motion relative to origin at each position
          // println("draw motion")
          // println(s"${origCur.position} ${origStates.last.position}")
          val relPositions = positions.zip(origStates).map({case (pos, orig) =>
            Vec3.add(origCur.position, Vec3.sub(pos, orig.position))})
          view.drawMotion(im, relPositions, factionColor, true, verticals = motionVerticals)

          vel
        }

        case _ => {
          // draw origin and destination states
          if (true) {
            val destStates = ticks.map(tick => flight.dest(tick))
            view.drawMotion(im, origStates.map(_.position), factionColor, true, verticals = motionVerticals)
            view.drawMotion(im, destStates.map(_.position), factionColor, true, verticals = motionVerticals)
          }

          // draw motion and velocity arrow
          view.drawMotion(im, positions, factionColor, true, verticals = motionVerticals)
          val vel = drawVelocityArrow(im, view, curDateJulian, flightFn, factionColor)

          // draw flight radii
          RenderFlight.drawFlightRadii(im, flight, curDateJulian, view)
          vel
        }

      }

      // draw status next to ship arrow
      // val pos2d = View.perspective(positions.last, camTrans, viewPos)    // wrong
      // TODO: make this optional in ShowSettings
      val pos2d = View.perspective(flightFn(curDateJulian), camTrans, viewPos)
      val (x, y) = Viewer.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)
      RenderFlight.drawSimpleFlightStatus(
        x, y,
        im,
        flight.ship,
        flight.faction,
        Color.GREEN,
        vel,
        viewerSettings
      )

    })


    fpOption.foreach(fp => {

      val (flightFn, ticks) = FlightParams.paramsToFun(fp)

      if (ticks.length < 1) {
        return scala.collection.mutable.Map()
      }

      val origStates = ticks.map(tick => fp.orig(tick))
      val destStates = ticks.map(tick => fp.dest(tick))
      val ticksSubset = ticks.takeWhile(x => x < curDateJulian).toList :+ curDateJulian
      val flightStates = ticksSubset.map(tick => flightFn(tick))

      // val factionColor = factions.getOrElse(fp.faction, Color.GREEN)
      // RenderFlight.highlightFlight(
      //   view, im, fp, factions,
      //   origStates, destStates, flightStates, flightColor)

      // prepare summary or description
      val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

      // average velocity
      val vel = distance / (fp.endDate.julian - fp.startDate.julian)

      // val curVel = drawVelocityArrow(im, view, curDateJulian, flightFn, factionColor)
      // TODO: option for relative velocity here
      val curVel = Vec3.length(stateFromFlightFn(curDateJulian, flightFn).velocity)

      if (statusOption == 0) {
        // draw flight status with current datetime, distance, and velocity
        RenderFlight.drawFlightStatus(
          im,
          fp.ship,
          fp.faction,
          Conversions.julianToCalendarDate(curDateJulian),
          Vec3.length(Vec3.sub(flightStates.last, flightStates.head)),
          curVel,
          viewerSettings)
      } else if (statusOption == 1) {
        // draw flight summary
        fp.ship match {
          case x: ConstAccelCraft => {
            val accel = flightFn match {
              case y: ConstAccelFlightFn => y.accel
              case _ => 0.0
            }
            RenderFlight.drawFlightSummary(
              im, x, fp.faction, distance, vel, accel,
              fp.origName, fp.destName, fp.startDate, fp.endDate,
              viewerSettings)
          }
          case x: ConstVelCraft => RenderFlight.drawFlightSummary(
            im, x, fp.faction, distance, vel,
            fp.origName, fp.destName, fp.startDate, fp.endDate,
            viewerSettings)
        }

      }

    })

    objects

  }


  def drawMeasurement(
      im: BufferedImage,
      view: Viewer,
      measurement: Measurement,
      flightFn: Option[FlightFn],
      julian: Double): Unit = {

    def get(x: String): Double => OrbitalState = {
      if (x.equals("Flight")) {
        // potentially get other flights
        flightFn.map(fn => {t: Double => ConvertFlight.flightState(fn, t)}).getOrElse(Locations.DefaultFun)
      } else {
        Locations(x)
      }
    }

    measurement match {
      case m: MeasurementLookupDistance => {
        val funcFst = get(m.fst)
        val funcSnd = get(m.snd)
        val fst = funcFst(julian)
        val snd = funcSnd(julian)

        val distAU = Vec3.length(Vec3.sub(snd.position, fst.position))
        val distKm = distAU * Conversions.AuToMeters * Conversions.MetersToKm

        val halfPerCvt = findHalfPerCvt(fst.position, snd.position, im, view)

        // render the measurement
        view.drawLine(im, fst.position, snd.position, MeasurementColor)
        simpleTable(
          (halfPerCvt._1 + measurement.dispOffset._1, halfPerCvt._2 + measurement.dispOffset._2),
          im,
          Seq(
            // (s"${m.fst} - ${m.snd}", false),
            (f"$distAU%.4f AU", false),
            (f"$distKm%.4f km", false)),
          MeasurementColor,
          view.settings)
      }

      case m: MeasurementLookupRelativeVelocity => {
        val funcFst = get(m.fst)
        val funcSnd = get(m.snd)
        val fst = funcFst(julian)
        val snd = funcSnd(julian)

        val velAUPerDay = Vec3.length(Vec3.sub(snd.velocity, fst.velocity))
        val velKmPerSec = (velAUPerDay
          * Conversions.AuToMeters
          * Conversions.MetersToKm
          / Conversions.DayToSec)

        val halfPerCvt = findHalfPerCvt(fst.position, snd.position, im, view)

        // render the measurement
        view.drawLine(im, fst.position, snd.position, MeasurementColor)
        simpleTable(
          (halfPerCvt._1 + measurement.dispOffset._1, halfPerCvt._2 + measurement.dispOffset._2),
          im,
          Seq(
            // (s"${m.fst} - ${m.snd}", false),
            (f"$velAUPerDay%.4f AU/day", false),
            (f"$velKmPerSec%.4f km/s", false)),
          MeasurementColor,
          view.settings)

      }

      case m: MeasurementFuncDistance => {
        val (fst, snd) = m.func(julian)

        val distAU = Vec3.length(Vec3.sub(snd, fst))
        val distKm = distAU * Conversions.AuToMeters * Conversions.MetersToKm

        val halfPerCvt = findHalfPerCvt(fst, snd, im, view)

        // render the measurement
        view.drawLine(im, fst, snd, MeasurementColor)
        simpleTable(
          (halfPerCvt._1 + measurement.dispOffset._1, halfPerCvt._2 + measurement.dispOffset._2),
          im,
          Seq(
            // (s"${m.fst} - ${m.snd}", false),
            (m.name, false),
            (f"$distAU%.4f AU", false),
            (f"$distKm%.4f km", false)),
          MeasurementColor,
          view.settings)

      }

    }

  }


  def findHalfPerCvt(fst: Vec3, snd: Vec3, im: BufferedImage, view: Viewer): (Int, Int) = {
    val half = Vec3.mul(Vec3.add(fst, snd), 0.5)
    val halfPer = View.perspective(half, view.camTrans, view.viewPos)
    Viewer.cvtPos(im, halfPer.x.toInt, halfPer.y.toInt)
  }


  // get info used for rendering spheres
  def sphereInfo(
      radiusZ: Double,
      trans: Mat44,
      camTrans: Mat44,
      viewPos: Vec3): (Vec3, Vec3, Vec2, Vec2, Double) = {

    // how big is the sphere in the viewport?
    // note that this could be calculated from a transformation without rotation

    val northPole = Transformations.transform(trans, Vec3(0.0, 0.0, radiusZ))
    val southPole = Transformations.transform(trans, Vec3(0.0, 0.0, -radiusZ))

    // draw axis
    // view.drawLine(im, northPole, southPole, Color.LIGHT_GRAY)

    val northPoleView = View.perspective(northPole, camTrans, viewPos)
    val southPoleView = View.perspective(southPole, camTrans, viewPos)
    val diameterView = Vec2.length(Vec2.sub(northPoleView, southPoleView))

    (northPole, southPole, northPoleView, southPoleView, diameterView)

  }

  // helper - draw velocity arrow given only t => position
  def drawVelocityArrow(
      im: BufferedImage,
      view: Viewer,
      curDateJulian: Double,
      flightFn: FlightFn,
      color: Color): Double = {

    if (curDateJulian - EpsVel > flightFn.startTime) {
      val curState = flightFn(curDateJulian)
      val curVelVec = Vec3.mul(
        Vec3.sub(curState, flightFn(curDateJulian - EpsVel)),
        1.0 / EpsVel)
      val curVel = Vec3.length(curVelVec)
      if (curVel > ConstVelFlightFn.VelMin) {
        view.drawArrow(im, OrbitalState(curState, curVelVec), color)
      }
      curVel
    } else {
      0.0
    }
  }


  def simpleTable(
    tableStart: (Int, Int),
    im: BufferedImage,
    texts: Seq[(String, Boolean)],
    color: Color,
    viewerSettings: ViewerSettings
  ): Unit = {

    val (tableStartX, tableStartY) = tableStart
    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    // someday I'm going to figure out how to properly generalize this code
    def table(desc: String, values: Seq[(String, Boolean)], row: Int, color: Color): Unit = {

      // draw header (not using for now)
      // gr.setColor(Color.GREEN)
      // gr.setFont(viewerSettings.displayFontSmall)
      // gr.drawString(desc, tableStartX, tableStartY + row * viewerSettings.lineHeightSmall)

      gr.setColor(color)

      values.zipWithIndex.foreach({case ((text, italic), idx) => {
        if (italic) {
          gr.setFont(viewerSettings.displayFontItalicSmall)
        } else {
          gr.setFont(viewerSettings.displayFontSmall)
        }
        gr.drawString(
          text,
          tableStartX, // + viewerSettings.columnWidthSmall,
          tableStartY + (row + idx) * viewerSettings.lineHeightSmall)
      }})
    }

    table("whatever:", texts, 0, color)

  }


  def stateFromFlightFn(
      curDateJulian: Double,
      flightFn: FlightFn
  ): OrbitalState = {
    val curPos = flightFn(curDateJulian)
    val curVel = Vec3.mul(
      Vec3.sub(curPos, flightFn(curDateJulian - EpsVel)),
      1.0 / EpsVel)
    OrbitalState(curPos, curVel)
  }

}