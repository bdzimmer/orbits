// Copyright (c) 2020 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import org.scalatest.FunSuite

class StyleTestSuite extends FunSuite {

  test("ShowSettings from string") {

    val default = Style.ViewerSettingsArtsy

    assert(
      Style.viewerSettingsFromString("").equals(default))

  }

}
