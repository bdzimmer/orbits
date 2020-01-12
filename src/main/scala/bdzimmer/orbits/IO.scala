// Copyright (c) 2018 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import java.awt.Color
import java.io.File

import scala.collection.Seq
import scala.collection.JavaConverters._
import scala.util.Try

import org.apache.commons.io.FileUtils

import bdzimmer.util.StringUtils._


object IO {

  // load factions from a CSV
  def loadFactions(inputFilename: String): Map[String, Color] = {
    val inputFile = new java.io.File(inputFilename)
    Try({
      val lines = FileUtils.readLines(inputFile)
      lines.asScala.map(line => {
        val splitted = line.split(",\\s+")
        val name = splitted(0)
        val color = new Color(
          splitted(1).toIntSafe(0),
          splitted(2).toIntSafe(0),
          splitted(3).toIntSafe(0))
        (name, color)
      }).toMap
    }).getOrElse(Map())
  }


  // load styles
  def loadStyles(inputFilename: String): Map[String, ViewerSettings] = {
    val inputFile = new java.io.File(inputFilename)
    Try({
      val lines = FileUtils.readLines(inputFile)
      lines.asScala.map(line => {
        val splitted = line.split(",")
        val name = splitted(0)
        (name, Style.viewerSettingsFromString(splitted(1)))
      }).toMap
    }).getOrElse(Map())
  }


  // load flights from a tab-separated file
  def loadFlightsTsv(inputFilename: String, ships: Map[String, Spacecraft]): List[FlightParams] = {
    val lines = FileUtils.readLines(new File(inputFilename))
    val defaultShip = ships.values.head
    lines.asScala.map(line => {
      val lineSplit = line.split("\\t")
      FlightParams(
        ships.getOrElse(lineSplit(0), defaultShip),
        lineSplit(1), // TODO: handle more complex location names, including lagrange points
        lineSplit(2),
        MeeusPlanets.Planets.getOrElse(lineSplit(1), MeeusPlanets.Earth).planet,
        MeeusPlanets.Planets.getOrElse(lineSplit(2), MeeusPlanets.Earth).planet,
        DateTime.parse(lineSplit(3)),
        DateTime.parse(lineSplit(4)),
        lineSplit(5).split(";\\s+").toList,
        lineSplit(6),
        if (lineSplit.length > 7) lineSplit(7) else "" // TODO: not sure if this is robust enough
      )
    }).toList
  }


  // save flights to a tab-separated file
  def saveFlightsTsv(flights: Seq[FlightParams], outputFilename: String): Unit = {
    val lines = flights.map(flight => {
      flight.ship.name + "\t" +
      flight.origName + "\t" +
      flight.destName + "\t" +
      flight.startDate.dateTimeString + "\t" +
      flight.endDate.dateTimeString + "\t" +
      flight.passengers.mkString("; ") + "\t" +
      flight.faction + "\t" +
      flight.description
    })
    FileUtils.writeLines(new File(outputFilename), lines.asJava)
  }


  // convert a flight to a secondary tag
  def flightToSecTag(fp: FlightParams): String = {
    "{{flight: " + fp.ship.name + " " + (List(
      "startLoc=" + fp.origName,
      "endLoc=" + fp.destName,
      "startDate=" + fp.startDate.dateTimeString,
      "endDate=" + fp.endDate.dateTimeString) ++ List(
      if (fp.passengers.nonEmpty) Some("passengers=" + fp.passengers.mkString("; ")) else None,
      if (!fp.faction.isEmpty) Some("faction=" + fp.faction) else None).flatten
    ).mkString(" | ") +
    "}}"
  }


  def saveFlightsSec(flights: Seq[FlightParams], outputFilename: String): Unit = {
    val lines = flights.map(flightToSecTag)
    FileUtils.writeLines(new File(outputFilename), lines.asJava)
  }

}