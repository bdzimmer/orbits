// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Generate an image of text with an executable.

package bdzimmer.orbits

import scala.collection.JavaConverters._

import java.io.File
import java.util
import java.awt.{Font, Graphics2D, RenderingHints, Color}
import java.awt.image.BufferedImage
import java.awt.font.TextAttribute

import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.apache.commons.imaging.{ImageFormats, Imaging}

case class TextConfig(text: String, font: String, size: Int)


object Text {

  val SizeDefault = 12
  val Kerning = true
  val ImageType = BufferedImage.TYPE_INT_ARGB

  val RenderHints = new RenderingHints(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  RenderHints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

  def readConfig(inputFilename: String): (String, Font) = {
    val lines = FileUtils.readLines(new File(inputFilename)).asScala
    val text = lines(0)
    val (font, _, _ , _) = Style.parseFont(lines(1))
    (text, font)
  }

  def main(argv: Array[String]): Unit = {
    val inputFilename = argv(0)
    val outputFilename = FilenameUtils.getBaseName(inputFilename) + ".png"

    val (text, font) = readConfig(inputFilename)
    val dummy = new BufferedImage(1, 1, ImageType)

    val renderFont = if (Kerning) {
      val map = new util.Hashtable[TextAttribute, AnyRef]
      map.put(TextAttribute.KERNING, TextAttribute.KERNING_ON)
      font.deriveFont(map)
    } else {
      font
    }

    // get metrics from dummy graphics instance
    val grDummy = dummy.getGraphics.asInstanceOf[Graphics2D]
    grDummy.setRenderingHints(Viewer.RenderHints)
    val metrics = grDummy.getFontMetrics(renderFont)
    val width = metrics.stringWidth(text)
    val height = metrics.getAscent + metrics.getDescent

    // allocate image of proper size
    val im = new BufferedImage(width, height, ImageType)
    val grRender = im.getGraphics.asInstanceOf[Graphics2D]
    grRender.setRenderingHints(RenderHints)

    // TODO: do you actually get subpixel resolution with the float verison?
    grRender.setColor(new Color(255, 255, 255))
    grRender.setFont(renderFont)
    grRender.drawString(text, 0, metrics.getAscent)

    Imaging.writeImage(im, new File(outputFilename), ImageFormats.PNG, new util.HashMap[String, Object]())

  }

}
