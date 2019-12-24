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

  val EpsVel = 1.0e-4  // for velocity calculation

  val DisplaySettings = Viewer.ViewerSettingsArtsy

  val MoonsExperiment = true


  def redraw(

      fpOption: Option[FlightParams],       // flight to highlight with status
      curDateJulian: Double,

      planets: Seq[(String, Planet)],
      flights: Seq[FlightParams],
      factions: Map[String, Color],

      asteroidBelt: Boolean,
      lagrangePoints: Boolean,
      orbitInfo: Boolean,
      motionVerticals: Boolean,

      statusOption: Int,

      camTrans: Mat44,
      viewPos: Vec3,

      im: BufferedImage): scala.collection.mutable.Map[String, Vec2] = {

    val objects: scala.collection.mutable.Map[String, Vec2] = new scala.collection.mutable.HashMap[String, Vec2]()

    val view = new Viewer(camTrans, viewPos, DisplaySettings)

    // helper - draw velocity arrow
    def drawVelocityArrow(flightFn: FlightFn, color: Color): Double = {
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

    val planetMotions = planets.map(x => {
      (x._1, Orbits.planetMotionPeriod(x._2.planet, curDateJulian))
    })

    // find other flights that are active at the same time as the current one
    val activeFlights = flights.filter(x =>
      // !x.equals(fp) &&
      x.startDate.julian <= curDateJulian && x.endDate.julian >= curDateJulian)

    // clear the image
    val gr = im.getGraphics
    gr.setColor(Color.BLACK)
    gr.fillRect(0, 0, im.getWidth, im.getHeight)

    // ~~~~

    // draw the grid and the sun
    view.drawGrid(im, GridLim, GridLim, None, new Color(0, 0, 128))
    val sun = Vec3(0.0, 0.0, 0.0)
    view.drawPosition(im, sun, "Sun", "", Color.YELLOW) // for now

    // draw the orbits of planets and their positions
    // the sequence of orbital states for each planet should start from same time
    // as the final state of the flight - this is the time that we are drawing the
    // flight at
    planetMotions.foreach(x => RenderFlight.drawOrbit(im, x._2, view, Color.LIGHT_GRAY, motionVerticals))

    // draw more detailed stuff if zoomed in far enough

    val detailMin =  100000.0
    val detailMax = 5000000.0

    if (viewPos.z > detailMin) {
      // TODO: better logic than the above

      planets.zip(planetMotions).foreach({case (x, y) => {
        val pos = y._2.last.position
        val radiusAu = x._2.radiusKm * 1000.0 / Conversions.AuToMeters
        val scale = Vec3(radiusAu, radiusAu, radiusAu)
        val name = x._1
        val rotation = Orbits.laplacePlaneICRFTransformation(
          x._2.axialTilt.rightAscension, x._2.axialTilt.declination)

        view.drawSphere(
          im,
          Transformations.transformation(rotation, y._2.last.position),
          scale,
          Color.LIGHT_GRAY)

        // TODO: draw axis

        val pos2d = View.perspective(pos, camTrans, viewPos)
        val gr = im.getGraphics.asInstanceOf[Graphics2D]
        gr.setRenderingHints(Viewer.RenderHints)
        val titleColor = Color.LIGHT_GRAY

        // coordinates in the viewer image
        val (ptx, pty) = view.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)

        if (name.length > 0 && view.inView(im, ptx, pty)) {
          // TODO: move sphere rendering in here

          gr.setFont(view.settings.displayFontLarge)

          // put the name of the planet near the planet
          // gr.drawString(name, ptx + rad, pty)

          // put the name of the planet dramatically near the bottom of the screen
          // TODO: this width could be cached
          val lineWidth = gr.getFontMetrics(view.settings.displayFontLarge).stringWidth(name)

          // fade the color in dramatically as a function of zoom
          val fade = math.min(((viewPos.z - detailMin) / (detailMax - detailMin)).toFloat, 1.0f)
          println(fade)
          gr.setColor(new Color(
            (titleColor.getRed * fade).toInt,
            (titleColor.getGreen * fade).toInt,
            (titleColor.getBlue * fade).toInt))

          gr.drawString(
            name.toUpperCase, im.getWidth / 2 - lineWidth / 2,
            im.getHeight - view.settings.lineHeightLarge)

        }

      }})
    } else {
      planetMotions.foreach(x => view.drawPosition(im, x._2.last.position, x._1, "", Color.LIGHT_GRAY))
    }

    planetMotions.foreach(x => {
      objects(x._1) = View.perspective(x._2.last.position, view.camTrans, view.viewPos)
    })

    // view.drawSphere(im, Transformations.IdentityTransformation, Vec3(0.5, 0.5, 0.5), Color.GREEN)

    // ~~~~ ~~~~ Experimentation with moons ~~~~ ~~~~

    if (MoonsExperiment) {
      Moons.Moons.foreach({case (name, moon) => {
        // TODO: remove ICRF transformation from calculation
        val laplacePlane = Some(
          moon.laplacePlane.map(
            y => Orbits.laplacePlaneICRFTransformation(y.rightAscension, y.declination)).getOrElse(Conversions.ICRFToEcliptic))
        val motion = Orbits.moonMotionPeriod(moon.primary, moon.moon, laplacePlane, curDateJulian)
        RenderFlight.drawOrbit(im, motion, view, Color.LIGHT_GRAY, motionVerticals)
        view.drawPosition(im, motion.last.position, name, "", Color.LIGHT_GRAY)

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

      }})

    }

    // ~~~~ ~~~~ ~~~~ ~~~~

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
      val (flightFn, ticks) = Editor.paramsToFun(flight)
      val positions = ticks.filter(x => x <= curDateJulian).map(tick => flightFn(tick))
      val factionColor = factions.getOrElse(flight.faction, Color.LIGHT_GRAY)

      if (true) {
        val origStates = ticks.map(tick => Orbits.planetState(flight.orig, tick))
        val destStates = ticks.map(tick => Orbits.planetState(flight.dest, tick))
        view.drawMotion(im, origStates.map(_.position), factionColor, true, verticals = motionVerticals)
        view.drawMotion(im, destStates.map(_.position), factionColor, true, verticals = motionVerticals)
      }

      // draw motion and velocity arrow
      view.drawMotion(im, positions, factionColor, true, verticals = motionVerticals)
      val vel = drawVelocityArrow(flightFn, factionColor)

      // draw flight radii
      RenderFlight.drawFlightRadii(im, flight, curDateJulian, view)

      // draw status
      // val pos2d = View.perspective(positions.last, camTrans, viewPos)    // wrong
      // TODO: make this optional in ShowSettings
      val pos2d = View.perspective(flightFn(curDateJulian), camTrans, viewPos)
      val (x, y) = view.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)
      RenderFlight.drawFlightStatus(
        x, y,
        im,
        flight.ship,
        flight.faction,
        Color.GREEN,
        Conversions.julianToCalendarDate(curDateJulian),
        Vec3.length(Vec3.sub(positions.last, positions.head)),
        vel,
        DisplaySettings
      )

    })


    fpOption.foreach(fp => {

      val (flightFn, ticks) = Editor.paramsToFun(fp)

      if (ticks.length < 1) {
        return scala.collection.mutable.Map()
      }

      val origStates = ticks.map(tick => Orbits.planetState(fp.orig, tick))
      val destStates = ticks.map(tick => Orbits.planetState(fp.dest, tick))
      val ticksSubset = ticks.takeWhile(x => x < curDateJulian)
      val flightStates = ticksSubset.map(tick => flightFn(tick))

      val factionColor = factions.getOrElse(fp.faction, Color.GREEN)

      // RenderFlight.highlightFlight(
      //   view, im, fp, factions,
      //   origStates, destStates, flightStates, flightColor)

      // prepare summary or description
      val distance = Vec3.length(
        Vec3.sub(destStates.last.position, origStates.head.position))

      // average velocity
      val vel = distance / (fp.endDate.julian - fp.startDate.julian)

      val curVel = drawVelocityArrow(flightFn, factionColor)

      if (statusOption == 0) {
        // draw flight status with current datetime, distance, and velocity
        RenderFlight.drawFlightStatus(
          im,
          fp.ship,
          fp.faction,
          Conversions.julianToCalendarDate(curDateJulian),
          Vec3.length(Vec3.sub(flightStates.last, flightStates.head)),
          curVel,
          DisplaySettings)
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
              DisplaySettings)
          }
          case x: ConstVelCraft => RenderFlight.drawFlightSummary(
            im, x, fp.faction, distance, vel,
            fp.origName, fp.destName, fp.startDate, fp.endDate,
            DisplaySettings)
        }

      }

    })

    objects

  }

}