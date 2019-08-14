// Copyright (c) 2019 Ben Zimmer. All rights reserved.

// Functionality for rendering movies.

package bdzimmer.orbits

import java.awt.Color

case class AnimationSettings(
  camPos: Vec3,
  fps: Int,
  interval: Double,
  damping: Double
  // TODO: other options such as canvas size
)


object Animation {

  // TODO: lots of extra stuff needs to go here
  def animateFlights(
      flights: List[FlightParams],   // all flights in epoch
      startDate: CalendarDateTime,
      endDate: CalendarDateTime,
      factions: Map[String, Color],
      showSettings: ShowSettings,
      animationSettings: AnimationSettings,
      outputDirname: String): Unit = {

    // find visible objects

    // find initial camera position
    // loop over julian dates by interval
      // calculate camera position
      // draw.redraw

  }

}
