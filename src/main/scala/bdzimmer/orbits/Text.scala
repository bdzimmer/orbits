// Copyright (c) 2020 Ben Zimmer. All rights reserved.

// Generate an image of text with an executable.

package bdzimmer.orbits

import scala.util.regexp
import scala.collection.mutable.{Buffer => MutableBuffer}
import scala.collection.JavaConverters._

import java.io.{File, FileWriter, PrintWriter}
import java.util
import java.awt.{BasicStroke, Color, Font, FontMetrics, Graphics2D, RenderingHints, Shape}
import java.awt.image.BufferedImage
import java.awt.font.{FontRenderContext, LineBreakMeasurer, TextAttribute}
import java.awt.geom.{AffineTransform, PathIterator}
import java.text.AttributedString

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

  case class SingleLineConfig(
    text: String,
    font: Font,
    borderX: Int,
    borderY: Int,
    stroke: Option[Double]
  )

  case class MultiLineConfig(
    paragraphs: List[String],
    font: Font,  // TODO: more fonts
    borderX: Int,
    borderY: Int,
    width: Int,
    height: Int,
    justify: Boolean
  )


  def main(argv: Array[String]): Unit = {

    val inputFilename = argv(0)
    val command = argv(1)
    val mode = argv.lift(2).getOrElse("single")

    val outputFilename = FilenameUtils.removeExtension(inputFilename) + ".png"
    val infoFilename = FilenameUtils.removeExtension(inputFilename) + "_info.txt"

    if (mode.equals("single")) {

      val config = readSingleLineConfig(inputFilename)

      val (metrics, width, height) = findMetrics(config.font, config.text)

      // only draw if the command is "draw"
      if (command.equals("draw")) {
        val im = drawSingle(config, width, height, metrics)
        writePng(im, outputFilename)
      }

      // save useful information that can be read into a dictionary
      val pw = new PrintWriter(new FileWriter(infoFilename))
      pw.println("ascent\t" + metrics.getAscent)
      pw.println("descent\t" + metrics.getDescent)
      pw.println("width\t" + width)
      pw.println("height\t" + height)
      pw.println("leading\t" + metrics.getLeading)
      pw.println("borderX\t" + config.borderX) // not sure if border should be part of this
      pw.println("borderY\t" + config.borderY) // but it can't hurt
      config.stroke.foreach(x => pw.println("stroke\t" + x))
      pw.close()

    } else if (mode.equals("multi")) {

      val config = readMultiLineConfig(inputFilename)
      val (metrics, _, _) = findMetrics(
        config.font, config.paragraphs.lift(0).getOrElse("test"))

      if (command.equals("draw")) {
        val im = drawMulti(config, metrics)
        writePng(im, outputFilename)
      }

    } else {
      println("unrecognized mode: " + mode)
    }

  }


  def drawSingle(config: SingleLineConfig, width: Int, height: Int, metrics: FontMetrics): BufferedImage = {

    // allocate image of proper size
    // note: may need to use maxAscent and maxDescent to create extra border
    val im = new BufferedImage(width + config.borderX * 2, height + config.borderY * 2, ImageType)
    val grRender = im.getGraphics.asInstanceOf[Graphics2D]
    grRender.setRenderingHints(RenderHints)

    grRender.setColor(Color.WHITE)
    grRender.setFont(config.font)

    // note how the y offset uses only border and ascent. we may also want to use leading.

    config.stroke match {
      case None => {
        grRender.drawString(config.text, 0 + config.borderX, metrics.getAscent + config.borderY)
      }
      case Some(strokeWidth) => {
        val stroke = new BasicStroke(
          strokeWidth.toFloat,
          BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)

        val glyphs = config.font.layoutGlyphVector(
          grRender.getFontRenderContext,
          config.text.toArray,
          0,
          config.text.length,
          Font.LAYOUT_LEFT_TO_RIGHT)

        grRender.translate(0 + config.borderX, metrics.getAscent + config.borderY)
        grRender.setStroke(stroke)

        val shape = glyphs.getOutline
        grRender.draw(shape)

        if (DebugSegments) {
          debugShape(shape, grRender, config.borderX, metrics.getAscent + config.borderY)
        }

      }
    }

    im

  }


  def drawMulti(config: MultiLineConfig, metrics: FontMetrics): BufferedImage = {
    // Draw multiline wrapped and possibly justified text onto an image.

    // This documentation is a good starting point:
    // https://docs.oracle.com/javase/7/docs/api/java/awt/font/LineBreakMeasurer.html

    val im = new BufferedImage(
      config.width + config.borderX * 2,
      config.height + config.borderY * 2,
      ImageType)

    val grRender = im.getGraphics.asInstanceOf[Graphics2D]
    grRender.setRenderingHints(RenderHints)
    val frc = grRender.getFontRenderContext

    var y: Float = config.borderY

    config.paragraphs.foreach(paragraph => {

      // for a simple, uniform font style
      // val styledText = new AttributedString(paragraph)
      // styledText.addAttribute(TextAttribute.FONT, config.font)
      // val styledText = parseStyles(paragraph, config.font)
      val styledText = parseStylesNew(paragraph, config.font)

      val count = breakCount(styledText, frc, config.width)

      val textIt = styledText.getIterator
      val measurer = new LineBreakMeasurer(textIt, frc)

      // while (measurer.getPosition < textIt.getEndIndex) {
      (0 until count).foreach(idx => {

        val layoutOrg = measurer.nextLayout(config.width)
        val layout = if (config.justify && idx < count - 1) {
          layoutOrg.getJustifiedLayout(config.width)
        } else {
          layoutOrg
        }

        // the layout metrics are potentially a little different
        // than the font metrics

        // y = y + layout.getAscent
        y = y + metrics.getAscent

        val dx = if (layout.isLeftToRight) {
          0.0f
        } else {
          config.width - layout.getAdvance
        }
        layout.draw(grRender, config.borderX + dx, y)

        // y = y + layout.getDescent + layout.getLeading
        y = y + metrics.getDescent + metrics.getLeading

      })
    })

    im
  }

  def breakCount(
      styledText: AttributedString,
      frc: FontRenderContext,
      width: Int
      ): Int = {

    var count = 0

    val textIt = styledText.getIterator
    val measurer = new LineBreakMeasurer(textIt, frc)

    while (measurer.getPosition < textIt.getEndIndex) {
      measurer.nextLayout(width)
      count = count + 1
    }

    count
  }


  def readSingleLineConfig(inputFilename: String): SingleLineConfig = {
    val lines = FileUtils.readLines(new File(inputFilename)).asScala

    // text on first line
    val text = lines(0)

    // font config on second line
    val ss = lines(1).split(";")

    // kerning enabled by default
    // val font = FontUtil.font(ss(0), FontUtil.getStyle(ss(1)), ss(2).toIntSafe())

    // kerning not enabled by default
    val font = new Font(ss(0), FontUtil.getStyle(ss(1)), ss(2).toIntSafe())

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

    // turn on kerning if called for
    val renderFont = if (Kerning) {
      FontUtil.enableKerning(font)
    } else {
      font
    }

    SingleLineConfig(text, renderFont, borderX, borderY, stroke)
  }


  def readMultiLineConfig(inputFilename: String): MultiLineConfig = {
    val lines = FileUtils.readLines(new File(inputFilename)).asScala

    // font config on first line
    val ss = lines(0).split(";")
    val font = new Font(ss(0), FontUtil.getStyle(ss(1)), ss(2).toIntSafe())

    // borders on second line
    val bs = lines(1).split(";")
    val borderX = bs(0).toIntSafe()
    val borderY = bs(1).toIntSafe()

    // width and height on third line
    val dims = lines(2).split(";")
    val width = dims(0).toIntSafe()
    val height = dims(1).toIntSafe()

    // justify mode on fourth line
    val justify = lines(3).equals("true")

    // lines of text on remaining lines
    val configLines = lines.drop(4).toList

    // turn on kerning if called for
    val renderFont = if (Kerning) {
      FontUtil.enableKerning(font)
    } else {
      font
    }

    MultiLineConfig(
      configLines, renderFont, borderX, borderY, width, height, justify)
  }


  def findMetrics(font: Font, text: String): (FontMetrics, Int, Int) = {

    // get metrics from dummy graphics instance
    val dummy = new BufferedImage(1, 1, ImageType)
    val grDummy = dummy.getGraphics.asInstanceOf[Graphics2D]
    grDummy.setRenderingHints(Viewer.RenderHints)
    val metrics = grDummy.getFontMetrics(font)

    // calculate width and height which (roughly) contains the drawn font
    val width = metrics.stringWidth(text)

    val height = metrics.getAscent + metrics.getDescent + metrics.getLeading

    (metrics, width, height)

  }


  def writePng(im: BufferedImage, outputFilename: String): Unit = {
    Imaging.writeImage(im, new File(outputFilename), ImageFormats.PNG, new util.HashMap[String, Object]())
  }


  def parseStyles(paragraph: String, font: Font): AttributedString = {
    // split by SINGLE SPACE, check beginnings of words for instructions
    var resultString = ""
    var attribs: MutableBuffer[(Font, Int, Int)] = MutableBuffer()
    var pos = 0

    paragraph.split(" ").foreach(word => {

      val trimmed = word.replaceAll("^\\s+","")
      val whitespaceLength = word.length - trimmed.length
      val whitespace = word.take(whitespaceLength)
      val (chunkString, chunkFont) = if (trimmed.startsWith("{i}")) {
        (trimmed.drop(3), font.deriveFont(Font.ITALIC))
      } else if (trimmed.startsWith("{b}")) {
        (trimmed.drop(3), font.deriveFont(Font.BOLD))
      } else if (trimmed.startsWith("{bi}")) {
        (trimmed.drop(4), font.deriveFont(Font.BOLD | Font.ITALIC))
      } else {
        (word, font)
      }

      if (whitespaceLength > 0) {
        resultString = resultString + whitespace
        attribs.append((font, pos, pos + whitespaceLength))
        pos = pos + whitespaceLength
      }

      if (chunkString.length > 0) {
        resultString = resultString + chunkString
        attribs.append((chunkFont, pos, pos + chunkString.length))
        pos = pos + chunkString.length
      }

      resultString = resultString + " "
      attribs.append((font, pos, pos + 1))
      pos = pos + 1

    })

    resultString = resultString.dropRight(1)
    attribs = attribs.dropRight(1)

    if (resultString.length > 0) {
      val result = new AttributedString(resultString)
      attribs.foreach(attrib => {
        result.addAttribute(TextAttribute.FONT, attrib._1, attrib._2, attrib._3)
      })
      result
    } else {
      val result = new AttributedString(" ")
      result.addAttribute(TextAttribute.FONT, font)
      result
    }

  }


  def parseStylesNew(paragraph: String, font: Font): AttributedString = {

    // in this new version, the tokens {b} and {i} enable or disable bold and italic
    // so we split by those tokens instead and have a loop that builds up the final
    // string by the chunks in between

    // split by SINGLE SPACE, check beginnings of words for instructions
    var resultString = ""
    val attribs: MutableBuffer[(Font, Int, Int)] = MutableBuffer()

    var isItalic = false
    var isBold = false

    val matcher = "\\{.*?\\}".r
    val toggles = matcher.findAllIn(paragraph).toList
    val chunksAll = matcher.split(paragraph).toList

    if (chunksAll.length > 1) {

      // section before first toggle
      resultString = chunksAll.headOption.getOrElse("")
      if (resultString.length > 0) {
        attribs.append((font, 0, resultString.length))
      }

      toggles.zip(chunksAll.tail).foreach({case (toggle, chunk) => {

        // update state using toggle
        if (toggle.startsWith("{i}")) {
          // toggle italic
          isItalic = !isItalic
        } else if (toggle.startsWith("{b}")) {
          // toggle bold
          isBold = !isBold
        }

        if (chunk.length > 0) {

          // append attribs and text
          var styledFont = font
          if (isItalic && !isBold) {
            styledFont = font.deriveFont(Font.ITALIC)
          } else if (isBold && !isItalic) {
            styledFont = font.deriveFont(Font.BOLD)
          } else if (isBold && isItalic) {
            styledFont = font.deriveFont(Font.ITALIC | Font.BOLD)
          }
          val pos = resultString.length
          attribs.append((styledFont, pos, pos + chunk.length))
          resultString = resultString + chunk
          // println(chunk + " " + pos + " " + pos + chunk.length)
        }

      }})
    } else {
      resultString = paragraph
      attribs.append((font, 0, paragraph.length))
    }

    if (resultString.length > 0) {
      val result = new AttributedString(resultString)
      attribs.foreach(attrib => {
        result.addAttribute(TextAttribute.FONT, attrib._1, attrib._2, attrib._3)
      })
      result
    } else {
      val result = new AttributedString(" ")
      result.addAttribute(TextAttribute.FONT, font)
      result
    }

  }


  def debugShape(shape: Shape, grRender: Graphics2D, xStart: Double, yStart: Double): Unit = {
    var x = xStart
    var y = yStart
    val pit = shape.getPathIterator(new AffineTransform())
    var xm: Double = 0.0
    var ym: Double = 0.0
    grRender.setColor(Color.GREEN)
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
        grRender.setColor(Color.RED)
        grRender.drawLine(
          x.toInt, y.toInt,
          xm.toInt, ym.toInt)
        x = xm
        y = ym
      } else if (segType == PathIterator.SEG_LINETO) {
        println("SEG_LINETO")
        grRender.setColor(Color.BLUE)
        grRender.drawLine(
          x.toInt, y.toInt,
          coords(0).toInt, coords(1).toInt)
        x = coords(0)
        y = coords(1)
      } else if (segType == PathIterator.SEG_QUADTO) {
        println("SEG_QUADTO")
        grRender.setColor(Color.GREEN)
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
        grRender.setColor(Color.GREEN)
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
