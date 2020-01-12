// Copyright (c) 2020 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import org.scalatest.FunSuite

import java.awt.Font

class StyleTestSuite extends FunSuite {

  test("ShowSettings from string") {

    // ~~~~ no modifications

    val default = Style.ViewerSettingsDefault

    assert(
      Style.viewerSettingsFromString("").equals(default))

    // ~~~~ change font

    val alt = Style.viewerSettingsFromString("displayFontLarge~Monospaced;plain;64;72")

    print(alt.displayFontLarge.getFontName)

    assert(alt.displayFontLarge.getFamily.equals("Monospaced"))
    assert(alt.displayFontLarge.getStyle.equals(Font.PLAIN))
    assert(alt.displayFontLarge.getSize.equals(64))
  }

}
