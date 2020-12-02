package bdzimmer.orbits

import java.awt.Color

object Simulation {

  def main(argv: Array[String]): Unit = {
    print("Hello World!")

    // set up interactive viewer for display

    // TODO: make a little JFrame with some controls

    val showSettings = Editor.ShowSettingsDefault
    val planets = showSettings.planets.filter(_._2).flatMap(
      x => MeeusPlanets.Planets.get(x._1).map(y => (x._1, y))).toList
    val viewerSettings = Style.ViewerSettingsDefault

    def getCurDateJulian(): Double = {
      0.0
    }

    val flights: scala.collection.mutable.Buffer[FlightParams] = scala.collection.mutable.Buffer()
    val factions = Map(("Default", Color.GREEN))

    val iv = new InteractiveView(
      "Orbits Edtior",
      getCurDateJulian,
      () => true,  // timeline mode will be false once we have a flight
      () => planets,
      () => flights,
      () => null,  // bruh
      () => factions,
      () => false,
      () => true,
      showSettings,
      viewerSettings
    )

  }

}
