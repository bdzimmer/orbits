// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Configurations and functions for cool styles

package bdzimmer.orbits

import java.awt.{BasicStroke, Font, Stroke}

import bdzimmer.util.StringUtils._


case class ViewerSettings(

    // standard font used for:
    // - planet names
    // - detailed status
    // - orbital infp
    displayFont: Font,
    displayFontItalic: Font,
    lineHeight: Int,
    columnWidth: Int,

    // small font used for:
    // - ship names and info
    displayFontSmall: Font,
    displayFontItalicSmall: Font,
    lineHeightSmall: Int,
    columnWidthSmall: Int,

    // large font used for:
    // - planet title on zoom-in
    displayFontLarge: Font,
    lineHeightLarge: Int,

    stroke: Stroke,        // stroke for lines

    circleRadius: Int,     // radius of circle used for planets and lagrange points
    arrows3D: Boolean,     // whether or not use 3D arrows
    arrowLength: Double    // arrow size
)


// TODO: make these fields constant
case class ShowSettings(
   var planets: scala.collection.immutable.Map[String, Boolean],
   var lagrangePoints: Boolean,
   var asteroidBelt: Boolean,
   var orbitInfo: Boolean,
   var motionVerticals: Boolean,
   var flightStatus: Int      // TODO: enumeration
)


object Style {

  // TODO: constants for font names / types
  // TODO: find proper monospace font

  // TODO: rename to "plain"
  val ViewerSettingsDefault = ViewerSettings(
    displayFont = new Font(Font.MONOSPACED, Font.BOLD, 12),
    displayFontItalic = new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, 12),
    lineHeight = 14,
    columnWidth = 100,

    displayFontSmall = new Font(Font.MONOSPACED, Font.BOLD, 12),
    displayFontItalicSmall = new Font(Font.MONOSPACED, Font.BOLD | Font.ITALIC, 12),
    lineHeightSmall = 14,
    columnWidthSmall = 100,

    displayFontLarge = new Font(Font.MONOSPACED, Font.BOLD, 48),
    lineHeightLarge = 56,

    stroke = new BasicStroke(2),

    circleRadius = 6,
    arrows3D = false,
    arrowLength = 0.0
  )


  val ViewerSettingsArtsy = ViewerSettings(
    displayFont = new Font(Fonts.FontPlay, Font.PLAIN, 16),
    displayFontItalic = new Font(Fonts.FontPlay, Font.PLAIN | Font.ITALIC, 16),
    lineHeight = 18,
    columnWidth = 125,

    displayFontSmall = new Font(Fonts.FontPlay, Font.PLAIN, 10),
    displayFontItalicSmall = new Font(Fonts.FontPlay, Font.PLAIN | Font.ITALIC, 10),
    lineHeightSmall = 11,
    columnWidthSmall = 60,

    displayFontLarge = new Font(Fonts.FontOrbitron, Font.BOLD, 64),
    lineHeightLarge = 72,

    stroke = new BasicStroke(2),

    circleRadius = 6,
    arrows3D = true,
    arrowLength = 100.0
  )


  def viewerSettingsFromString(s: String): ViewerSettings = {
    var settings = ViewerSettingsArtsy

    if (s.equals("")) {
      settings
    } else {

      try { // feeling lazy
        s.split("\\s*\\|\\s*").foreach(change => {
          val cs = change.split("=")
          val key = cs(0)
          val value = cs(1)
          key match {
            case "displayFont" => {
              val (font, fontItalic, lineHeight, columnWidth) = parseFont(value)
              settings = settings.copy(
                displayFont = font,
                displayFontItalic = fontItalic,
                lineHeight = lineHeight,
                columnWidth = columnWidth
              )
            }
            case "displayFontSmall" => {
              val (font, fontItalic, lineHeight, columnWidth) = parseFont(value)
              settings = settings.copy(
                displayFontSmall = font,
                displayFontItalicSmall = fontItalic,
                lineHeightSmall = lineHeight,
                columnWidthSmall = columnWidth
              )
            }
            case "displayFontLarge" => {
              val (font, _, lineHeight, _) = parseFont(value)
              settings = settings.copy(
                displayFontLarge = font,
                lineHeightLarge = lineHeight
              )
            }
            case "stroke" => {
              settings = settings.copy(stroke = new BasicStroke(value.toIntSafe()))
            }
            case "circleRadius" => {
              settings = settings.copy(circleRadius = value.toIntSafe())
            }
            case "arrows3D" => {
              settings = settings.copy(arrows3D = value.toBooleanSafe)
            }
            case "arrowLength" => {
              settings = settings.copy(arrowLength = value.toDoubleSafe())
            }
            case _ => println("key '" + key + "' unknown for ViewerSettings")
          }

        })
      } catch {
        case e: Exception => {
          println("improperly formatted viewerSettings string!")
          print("\t" + e.toString)
        }
      }

      settings
    }

  }


  def parseFont(s: String): (Font, Font, Int, Int) = {
    // construct font and related line height and column width info
    // from a string like this:
    // name;style;size;height[;colwidth]

    // I'm being lazy and making assumptions; so this will definitely
    // throw an exception if the string isn't formatted correctly.

    val ss = s.split(";")

    val name = ss(0)
    val style = ss(1) match {
      case "bold" => Font.BOLD
      case "italic" => Font.ITALIC
      case "bolditalic" => Font.BOLD | Font.ITALIC
      case _ => Font.PLAIN
    }
    val size = ss(2).toIntSafe()
    val font = new Font(name, style, size)
    val fontItalic = font.deriveFont(font.getStyle | Font.ITALIC)

    // TODO: automatically derive these if they are not supplied
    val height = ss(3).toIntSafe()
    val colwidth = if (ss.length > 4) {
      ss(4).toIntSafe()
    } else {
      0
    }

    (font, fontItalic, height, colwidth)
  }

  // TODO: body names dictionary functions

}


object Fonts {
  // A couple of nice-looking fonts from Google Fonts

  val FontPlay = "Play"
  val FontOrbitron = "Orbitron"
}

