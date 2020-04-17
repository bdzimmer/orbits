// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Generate an image of text with an executable.

package bdzimmer.orbits

import scala.collection.JavaConverters._
import java.io.{File, FileWriter, PrintWriter}
import java.util
import java.awt.{BasicStroke, Color, Font, Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import java.awt.geom.{AffineTransform, PathIterator}
import java.awt.Shape

import org.apache.commons.io.{FileUtils, FilenameUtils}
import org.apache.commons.imaging.{ImageFormats, Imaging}

import bdzimmer.util.StringUtils._


object Text {

  val SizeDefault = 12
  val Kerning = true
  val ImageType: Int = BufferedImage.TYPE_INT_ARGB
  val DebugSegments = false

  val RenderHints = new RenderingHints(
    RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  RenderHints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

  def readConfig(inputFilename: String): (String, Font, (Int, Int), Option[Double]) = {
    val lines = FileUtils.readLines(new File(inputFilename)).asScala

    // text on first line
    val text = lines(0)

    // font config on second line
    val ss = lines(1).split(";")
    var font = FontUtil.font(ss(0), FontUtil.getStyle(ss(1)), ss(2).toIntSafe())

    // borders on third line
    val bs = lines(2).split(";")
    val borderX = bs(0).toIntSafe()
    val borderY = bs(1).toIntSafe()

    // optional stroke on fourth line
    val stroke = if (lines.length > 3) {
      Some(lines(3).toDoubleSafe())
    } else {
      None
    }

    // optional - scale
//    if (lines.length > 3) {
//      val scale = lines(3).toDoubleSafe()
//      if (scale > 0.0) {
//        val trans = new AffineTransform()
//        trans.setToScale(scale, scale)
//        font = font.deriveFont(trans)
//      }
//    }

    (text, font, (borderX, borderY), stroke)
  }

  def main(argv: Array[String]): Unit = {

    val inputFilename = argv(0)
    val outputFilename = FilenameUtils.removeExtension(inputFilename) + ".png"
    val infoFilename = FilenameUtils.removeExtension(inputFilename) + "_info.txt"

    val (text, font, (borderX, borderY), stroke) = readConfig(inputFilename)
    val dummy = new BufferedImage(1, 1, ImageType)

    val renderFont = if (Kerning) {
      FontUtil.enableKerning(font)
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
    // note: may need to use maxAscent and maxDescent to create extra border
    val im = new BufferedImage(width + borderX * 2, height + borderY * 2, ImageType)
    val grRender = im.getGraphics.asInstanceOf[Graphics2D]
    grRender.setRenderingHints(RenderHints)

    grRender.setColor(new Color(255, 255, 255))
    grRender.setFont(renderFont)

    stroke match {
      case None => {
        grRender.drawString(text, 0 + borderX, metrics.getAscent + borderY)
      }
      case Some(strokeWidth) => {
        val stroke = new BasicStroke(
          strokeWidth.toFloat,
          BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

        val glyphs = renderFont.layoutGlyphVector(
          grRender.getFontRenderContext,
          text.toArray,
          0,
          text.length,
          Font.LAYOUT_LEFT_TO_RIGHT)

        grRender.translate(0 + borderX, metrics.getAscent + borderY)
        grRender.setStroke(stroke)

        val shape = glyphs.getOutline
        grRender.draw(shape)

        if (DebugSegments) {
          debugShape(shape, grRender, borderX, metrics.getAscent + borderY)
        }

      }
    }


    Imaging.writeImage(im, new File(outputFilename), ImageFormats.PNG, new util.HashMap[String, Object]())

    // save other useful information that can be read into a dictionary
    val pw = new PrintWriter(new FileWriter(infoFilename))
    pw.println("ascent\t" + metrics.getAscent)
    pw.println("descent\t" + metrics.getDescent)
    pw.println("width\t" + width)
    pw.println("height\t" + height)
    pw.println("borderX\t" + borderX)  // not sure if border should be part of this
    pw.println("borderY\t" + borderY)  // but it can't hurt
    pw.println("stroke\t" + stroke)
    pw.close()

  }


  def debugShape(shape: Shape, grRender: Graphics2D, xStart: Double, yStart: Double): Unit = {
    var x = xStart
    var y = yStart
    val pit = shape.getPathIterator(new AffineTransform())
    var xm: Double = 0.0
    var ym: Double = 0.0
    grRender.setColor(new Color(0, 255, 0))
    while (!pit.isDone) {
      val coords = new Array[Double](6)
      val segType = pit.currentSegment(coords)
      println(
        coords(0) + " " + coords(1) + " " +
        coords(2) + " " + coords(3) + " " +
        coords(4) + " " + coords(5))
      if (segType == PathIterator.SEG_MOVETO) {
        println("SEG_MOVETO")
        x = coords(0)
        y = coords(1)
        xm = coords(0)
        ym = coords(1)
      } else if (segType == PathIterator.SEG_CLOSE) {
        println("SEG_CLOSE")
        grRender.setColor(new Color(255, 0, 0))
        grRender.drawLine(
          x.toInt, y.toInt,
          xm.toInt, ym.toInt)
        x = xm
        y = ym
      } else if (segType == PathIterator.SEG_LINETO) {
        println("SEG_LINETO")
        grRender.setColor(new Color(0, 0, 255))
        grRender.drawLine(
          x.toInt, y.toInt,
          coords(0).toInt, coords(1).toInt)
        x = coords(0)
        y = coords(1)
      } else if (segType == PathIterator.SEG_QUADTO) {
        println("SEG_QUADTO")
        grRender.setColor(new Color(0, 255, 0))
        grRender.drawLine(
          x.toInt, y.toInt,
          coords(0).toInt, coords(1).toInt)
        grRender.drawOval(
          coords(0).toInt - 4, coords(1).toInt - 4, 8, 8)
        grRender.drawLine(
          coords(0).toInt, coords(1).toInt,
          coords(2).toInt, coords(3).toInt)
        x = coords(2)
        y = coords(3)
      } else if (segType == PathIterator.SEG_CUBICTO) {
        println("SEG_CUBICTO")
        grRender.setColor(new Color(0, 255  , 0))
        grRender.drawLine(
          x.toInt, y.toInt,
          coords(0).toInt, coords(1).toInt)
        grRender.drawLine(
          coords(0).toInt, coords(1).toInt,
          coords(2).toInt, coords(3).toInt)
        grRender.drawLine(
          coords(2).toInt, coords(3).toInt,
          coords(4).toInt, coords(5).toInt)
        x = coords(4)
        y = coords(5)
      } else {
        println("other!")
      }
      pit.next()
    }
  }

}
