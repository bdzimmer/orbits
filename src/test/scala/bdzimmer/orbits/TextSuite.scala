// Copyright (c) 2020 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import org.scalatest.FunSuite

import java.awt.Font

import bdzimmer.orbits.Text.{SingleLineConfig, MultiLineConfig}


class TextSuite extends FunSuite {

  test("single line text") {

    val borderSize = 32
    val config = SingleLineConfig(
      "AVIARY",
      new Font(Font.SERIF, Font.PLAIN, 64),
      borderSize, borderSize,
      None
    )

    val (metrics, width, height) = Text.findMetrics(config.font, config.text)
    val im = Text.drawSingle(config, width, height, metrics)

    // some exact values (these may not work on all platforms)
    assert(im.getWidth() == 317)
    assert(im.getHeight() == 140)

    // I suspect the leading of most English fonts is 0
    // since leading hasn't been an issue before now with ignoring it
    assert(metrics.getLeading == 0)
    assert(height == metrics.getAscent + metrics.getDescent)

    assert(metrics.getAscent + metrics.getDescent == im.getHeight - 2 * borderSize)
    assert(width == im.getWidth - 2 * borderSize)

    Text.writePng(im, "test_single_serif.png")

  }


  test("multiline text") {

    val borderSize = 32
    val width = 1920
    val height = 1080
    val paragraphs = List(  // each of these will be one paragraph
      "  Lorem {b}ipsum{b} {i}{b}dolo{b}{i}r {i}sit{i} amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.",
      " ",
      "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.",
      "Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.",
      "Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
    )

    val config = MultiLineConfig(
      paragraphs,
      // new Font(Font.SERIF, Font.PLAIN, 64),
      new Font("Play", Font.PLAIN, 64),
      borderSize, borderSize,
      width, height,
      true
    )

    val (metrics, _, _) = Text.findMetrics(config.font, " ")
    val im = Text.drawMulti(config, metrics)

    assert(im.getWidth == width + borderSize * 2)
    assert(im.getHeight == height + borderSize * 2)

    Text.writePng(im, "test_multi_serif.png")
  }

}
