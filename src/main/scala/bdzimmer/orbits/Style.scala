// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Configurations and functions for cool styles

package bdzimmer.orbits

import java.awt.{BasicStroke, Font, Stroke}


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

  val ViewerSettingsDefault = ViewerSettings(
    displayFont = new Font("Monospace", Font.BOLD, 12),
    displayFontItalic = new Font("Monospace", Font.BOLD | Font.ITALIC, 12),
    lineHeight = 14,
    columnWidth = 100,

    displayFontSmall = new Font("Monospace", Font.BOLD, 12),
    displayFontItalicSmall = new Font("Monospace", Font.BOLD | Font.ITALIC, 12),
    lineHeightSmall = 14,
    columnWidthSmall = 100,

    displayFontLarge = new Font("Monospace", Font.BOLD, 48),
    lineHeightLarge = 56,

    stroke = new BasicStroke(2),

    circleRadius = 6,
    arrows3D = false,
    arrowLength = 0.0
  )

  // redefine as a copy of the default
  val ViewerSettingsArtsy = ViewerSettings(
    // displayFont = new Font("Orbitron", Font.BOLD, 16),
    // displayFontItalic = new Font("Orbitron", Font.BOLD | Font.ITALIC, 16),

    displayFont = new Font("Play", Font.PLAIN, 16),
    displayFontItalic = new Font("Play", Font.PLAIN | Font.ITALIC, 16),
    lineHeight = 18,
    columnWidth = 125,

    displayFontSmall = new Font("Play", Font.PLAIN, 10),
    displayFontItalicSmall = new Font("Play", Font.PLAIN | Font.ITALIC, 10),
    lineHeightSmall = 11,
    columnWidthSmall = 60,

    displayFontLarge = new Font("Orbitron", Font.BOLD, 64),
    lineHeightLarge = 72,

    stroke = new BasicStroke(2),

    circleRadius = 6,
    arrows3D = true,
    arrowLength = 100.0
  )

  // TODO: function for constructing a new ViewerSettings object from a string
  // this would allow it to be configured
  // loop with a sequence of transformations of a default

}
