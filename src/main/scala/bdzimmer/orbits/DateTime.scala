// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Date / DateTime conversions and parsing.

package bdzimmer.orbits

import bdzimmer.util.{Result, Pass, Fail}


case class CalendarDateTime(
    year: Int,
    month: Int,
    day: Int,
    hours: Int = 0,
    minutes: Int = 0,
    seconds: Double = 0.0) {

  val julian: Double = {
    val JGREG = 15 + 31 * (10 + 12 * 1582)

    val dayFrac = day + (hours / 24.0) + (minutes / 1440.0) + (seconds / 86400.0)

    val (yearMod, monthMod) = if (month < 3) {
      (year - 1, month + 12)
    } else {
      (year, month)
    }

    val b = if (day + 31 * (monthMod + 12 * yearMod) >= JGREG) {
      val a = math.floor(yearMod / 100)
      2 - a + math.floor(a / 4)
    } else {
      0.0
    }

    math.floor(365.25 * yearMod) + math.floor(30.6001 * (monthMod + 1)) + dayFrac + 1720994.5 + b
  }


  def dateTimeString: String = {
    val secondsInt = seconds.toInt
    f"$year%04d-$month%02d-$day%02d $hours%02d:$minutes%02d:$secondsInt%02d"
  }


  def dateString: String = {
    f"$year%04d-$month%02d-$day%02d"
  }


}



object DateTime {

  // TODO: rewrite as a case class, use tuple for ordering
  type DateTuple = (Int, Option[Int], Option[Int])

  val DefaultMonths = List(
    "january", "february", "march", "april",
    "may", "june", "july", "august",
    "september", "october", "november", "december")

  val DefaultParser = new DateTupleParser(DefaultMonths)

  def parse(dateTime: String, dtp: DateTupleParser = DefaultParser): CalendarDateTime = {
    val words = dateTime.split("\\s+").toSeq

    // if there is more than one word and the last word contains ":", then
    // a time string is present.
    val (date, time) = if (words.length > 1 && words.last.contains(":")) {
      (dtp.parse(words.dropRight(1).mkString(" ")),
       parseTime(words.last))
    } else {
      (dtp.parse(dateTime), (0, 0, 0.0))
    }

    CalendarDateTime(
        date._1,
        date._2.getOrElse(1),
        date._3.getOrElse(1),
        time._1,
        time._2,
        time._3)
  }


  def parseTime(time: String): (Int, Int, Double) = {
    val parseResult = time.split("\\:").toList match {
      case hours :: minutes :: seconds :: Nil =>
        Result((hours.toInt, minutes.toInt, seconds.toDouble))
      case hours :: minutes :: Nil =>
        Result((hours.toInt, minutes.toInt, 0.0))
      case _ =>
        Result((0, 0, 0.0))
    }
    parseResult match {
      case Pass(x) => x
      case Fail(msg) => (0, 0, 0.0) // default date for now
    }
  }


  def calendarDateTime(dt: DateTuple): CalendarDateTime = {
    // assumes the date tuple represents a Gregorian date
    CalendarDateTime(dt._1, dt._2.getOrElse(0) + 1, dt._3.getOrElse(1))
  }

}


class DateTupleParser(months: Seq[String]) {

  import DateTime.DateTuple

  def parse(date: String): DateTuple = {
    val parseResult = if (date.contains("-") || date.contains("/")) {
      parseNumber(date)
    } else {
      parseName(date)
    }
    parseResult match {
      case Pass(x) => x
      case Fail(msg) => (0, Some(1), Some(1)) // default date for now
    }
  }


  // Parse a date string of one of the following forms:
  // - month day year
  // - month year
  // - year
  // where month is a month name and day / year are integers.
  // The parts of the year may be separated by a mixture of whitespace
  // and commas.

  // TODO: revise and test the parsing regex

  def parseName(date: String): Result[String, DateTuple] = {
    date.split("\\s*[,\\s]+").toList match {
      case month :: day :: year :: Nil =>
        Result((year.toInt, Some(parseMonth(month)), Some(day.toInt)))
      case month :: year :: Nil =>
        Result((year.toInt, Some(parseMonth(month)), None))
      case year :: Nil =>
        Result((year.toInt, None, None))
      case _ => Fail("Invalid date specification")
    }
  }


  // Parse a date string of one of the following form:
  // - year month day
  // - year month
  // - year
  // where year, month, and day are integers.
  // The parts of the year may be separated by a hyphen or forward slash.

  def parseNumber(date: String): Result[String, DateTuple] = {
    date.split("[-/]").toList match {
      case year :: month :: day :: Nil =>
        Result((year.toInt, Some(month.toInt), Some(day.toInt)))
      case year :: month :: Nil =>
        Result((year.toInt, Some(month.toInt), None))
      case year :: Nil =>
        Result((year.toInt, None, None))
      case _ => Fail("Invalid date specification")
    }
  }


  def render(date: DateTuple): String = date match {
    case (year, Some(month), Some(day)) =>  months(month - 1).capitalize + " " + day + ", " + year
    case (year, Some(month), None) => months(month - 1).capitalize + " " + year
    case (year, None, None) => year.toString
    case _ => "Invalid DateTuple"
  }


  // get the month name of a month number (starting at 1)
  def month(i: Int): String = {
    val mi = i - 1
    if (mi < 0 || mi > months.length - 1) {
      "INVALID MONTH"
    } else {
      months(mi)
    }

  }


  private def parseMonth(month: String): Int = {
    val index = months.indexWhere(x => x.startsWith(month.toLowerCase))
    if (index == -1) 1 else index + 1
  }

}