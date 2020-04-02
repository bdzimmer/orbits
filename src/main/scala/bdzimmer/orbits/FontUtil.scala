// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// A class for font utilities.

package bdzimmer.orbits

import java.awt.Font
import java.awt.font.TextAttribute

object FontUtil {

  // create a font with kerning enabled by default
  def font(name: String, style: Int, size: Int): Font = {
    val fontNoKerning = new Font(name, style, size)
    enableKerning(fontNoKerning)
    fontNoKerning
  }


  def enableKerning(font: Font): Font = {
    // TODO: investigate if there is a better way to do this
    val map = new java.util.Hashtable[TextAttribute, AnyRef]()
      map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON)
      font.deriveFont(map)
  }


  def getStyle(style: String): Int = style match {
    case "bold" => Font.BOLD
    case "italic" => Font.ITALIC
    case "bolditalic" => Font.BOLD | Font.ITALIC
    case _ => Font.PLAIN
  }

}
