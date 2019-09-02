// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Render 3D positions and motions to 2D images.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import java.awt.image.BufferedImage
import java.awt.{Color, Font, RenderingHints, Graphics2D}


class Viewer(val camTrans: Mat44, val viewPos: Vec3, val settings: ViewerSettings) {

  def drawGrid(im: BufferedImage, gridLim: Int, color: Color): Unit = {

    val spacing = 1.0
    val xRange = (-gridLim.toDouble to gridLim by spacing)
    val yRange = xRange

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)

    def drawSegment(pt1: Vec3, pt2: Vec3): Unit = {
      val start = View.perspective(pt1, camTrans, viewPos)
      val end   = View.perspective(pt2, camTrans, viewPos)
      val x1 = start.x.toInt + im.getWidth / 2
      val y1 = im.getHeight - (start.y.toInt + im.getHeight / 2)
      val x2 = end.x.toInt + im.getWidth / 2
      val y2 = im.getHeight - (end.y.toInt + im.getHeight / 2)
      // println(x1 + " " + y1 + " " + x2 + " " + y2)
      // don't draw wildly inappropriate values
      if (x1.abs < Viewer.DrawMax && y1.abs < Viewer.DrawMax && x2.abs < Viewer.DrawMax  && y2.abs < Viewer.DrawMax) {
        gr.drawLine(x1, y1, x2, y2)
      }
    }

    // draw segments going in the x direction
    yRange.foreach(y => {
      xRange.dropRight(1).foreach(x => {
        drawSegment(Vec3(x, y, 0), Vec3(x + spacing, y, 0))
      })
    })

    // draw segments going in the y direction
    xRange.foreach(x => {
      yRange.dropRight(1).foreach(y => {
        drawSegment(Vec3(x, y, 0), Vec3(x, y + spacing, 0))
      })
    })

  }


  def drawMotion(
      im: BufferedImage,
      pos: Seq[Vec3],
      color: Color,
      lines: Boolean = true,
      verticals: Boolean = false): Unit = {

    val pos2d = pos.map(p => View.perspective(p, camTrans, viewPos))

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)

    if (!lines) {
      val colInt = color.getRGB
      pos2d.foreach(p => {
        val x = p.x.toInt + im.getWidth / 2
        val y = im.getHeight - (p.y.toInt + im.getHeight / 2)
        if (x >= 0 && x < im.getWidth && y >= 0 && y < im.getHeight) {
          im.setRGB(x, y, colInt)
        }
      })
    } else {
      gr.setColor(color)
      if (pos2d.length > 1) {
        pos2d.zipWithIndex.dropRight(1).foreach({case (p1, idx) => {
          val (x1, y1) = cvtPos(im, p1.x.toInt, p1.y.toInt)  // TODO: experiment with round
          val p2 = pos2d(idx + 1)
          val (x2, y2) = cvtPos(im, p2.x.toInt, p2.y.toInt)
          if (x1.abs < Viewer.DrawMax && y1.abs < Viewer.DrawMax && x2.abs < Viewer.DrawMax && y2.abs < Viewer.DrawMax) {
            gr.drawLine(x1, y1, x2, y2)
          }
        }})
      }
    }

    if (verticals) {
      val colorWithAlpha = new Color(
        color.getRed, color.getGreen, color.getBlue, Math.min(128, color.getAlpha))
      gr.setColor(colorWithAlpha)

      if (pos.length > 1) {
        pos.zipWithIndex.dropRight(1).foreach({case (pr0, idx) => {

          val p0 = View.perspective(pr0, camTrans, viewPos)
          val pr0_b = Vec3(pr0.x, pr0.y, 0.0)
          val p0_b = View.perspective(pr0_b, camTrans, viewPos)

          val pr1 = pos(idx + 1)
          val p1 = View.perspective(pr1, camTrans, viewPos)
          val pr1_b = Vec3(pr1.x, pr1.y, 0.0)
          val p1_b = View.perspective(pr1_b, camTrans, viewPos)


          val (x0, y0) = cvtPos(im, p0.x.toInt, p0.y.toInt)
          val (x0_b, y0_b) = cvtPos(im, p0_b.x.toInt, p0_b.y.toInt)
          val (x1, y1) = cvtPos(im, p1.x.toInt, p1.y.toInt)
          val (x1_b, y1_b) = cvtPos(im, p1_b.x.toInt, p1_b.y.toInt)

          gr.fillPolygon(
              Array(x0, x0_b, x1_b, x1),
              Array(y0, y0_b, y1_b, y1),
             4
          )


        }})
      }

      /*
      pos.foreach(pr => {
        val p = View.perspective(pr, camTrans, viewPos)
        val pr0 = Vec3(pr.x, pr.y, 0.0)
        val p2 = View.perspective(pr0, camTrans, viewPos)
        val x = p.x.toInt + im.getWidth / 2
        val y = im.getHeight - (p.y.toInt + im.getHeight / 2)
        val x2 = p2.x.toInt + im.getWidth / 2
        val y2 = im.getHeight - (p2.y.toInt + im.getHeight / 2)
        gr.drawLine(x, y, x2, y2)
      })
      */

    }

  }


  def cvtPos(im: BufferedImage, x: Int, y: Int): (Int, Int) = {
    (x + im.getWidth / 2, im.getHeight - (y + im.getHeight / 2))
  }

  def drawPosition(
      im: BufferedImage, pos: Vec3, name: String, desc: String,
      color: Color, fill: Boolean = true): Unit = {

    val pos2d = View.perspective(pos, camTrans, viewPos)
    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)

    val x = pos2d.x.toInt + im.getWidth / 2
    val y = im.getHeight - (pos2d.y.toInt + im.getHeight / 2)

    val rad = settings.circleRadius
    if (fill) {
      gr.fillOval(x - rad, y - rad, rad * 2, rad * 2)
    } else {
      gr.drawOval(x - rad, y - rad, rad * 2, rad * 2)
    }
    gr.setFont(settings.displayFont)
    gr.drawString(name, x + rad, y + settings.lineHeight)
    gr.drawString(desc, x + rad, y + settings.lineHeight * 2)
  }


  def drawPolygon(im: BufferedImage, polygon: Seq[Vec2], color: Color): Unit = {
    val ptsShifted = polygon.map(p => cvtPos(im, p.x.toInt, p.y.toInt))
    if (!ptsShifted.exists(p => p._1.abs > Viewer.DrawMax || p._2.abs > Viewer.DrawMax)) {
      val gr = im.getGraphics.asInstanceOf[Graphics2D]
      gr.setRenderingHints(Viewer.RenderHints)
      gr.setColor(color)
      gr.fillPolygon(
        ptsShifted.map(_._1).toArray,
        ptsShifted.map(_._2).toArray,
        polygon.length)
    }
  }


  def drawArrow(im: BufferedImage, os: OrbitalState, color: Color): Unit = {
    if (!settings.arrows3D) {
      val position = View.perspective(os.position, camTrans, viewPos)
      val direction = Vec2.normalize(Vec2.sub(
        View.perspective(Vec3.add(os.position, Vec3.normalize(os.velocity)), camTrans, viewPos),
        position))
      val arrowPoints = Viewer.arrowPoints(position, direction)
      drawPolygon(im, arrowPoints, color)
    } else {
      val arrowPoints3d = Viewer.arrowPoints3D(
        os.position, Vec3.normalize(os.velocity), settings.arrowLength / viewPos.z)
      val arrowPoints = arrowPoints3d.map(x => View.perspective(x, camTrans, viewPos))
      drawPolygon(im, arrowPoints, color)
    }

  }


  def drawRing(im: BufferedImage, r0: Double, r1: Double, color: Color): Unit = {
    val res = (math.Pi * 2.0) / 90.0

    val r0pts = (0.0 to math.Pi * 2.0 by res).map(x =>
      Vec3(r0 * math.cos(x), r0 * math.sin(x), 0.0))
    val r1pts = (0.0 to math.Pi * 2.0 by res).map(x =>
      Vec3(r1 * math.cos(x), r1 * math.sin(x), 0.0))

    for (idx <- (1 until r0pts.length)) {
      val tri0 = Seq(r0pts(idx), r1pts(idx), r1pts(idx - 1))
      drawPolygon(im, tri0.map(View.perspective(_, camTrans, viewPos)), color)
      val tri1 = Seq(r0pts(idx), r1pts(idx - 1), r0pts(idx - 1))
      drawPolygon(im, tri1.map(View.perspective(_, camTrans, viewPos)), color)
    }
  }

}


case class ViewerSettings(
    displayFont: Font,
    displayFontItalic: Font,
    lineHeight: Int,
    columnWidth: Int,

    displayFontSmall: Font,
    displayFontItalicSmall: Font,
    lineHeightSmall: Int,
    columnWidthSmall: Int,

    circleRadius: Int,
    arrows3D: Boolean,
    arrowLength: Double
)


object Viewer {

  val DrawMax = 32768

  val ViewerSettingsDefault = ViewerSettings(
    displayFont = new Font("Monospace", Font.BOLD, 12),
    displayFontItalic = new Font("Monospace", Font.BOLD | Font.ITALIC, 12),
    lineHeight = 14,
    columnWidth = 100,

    displayFontSmall = new Font("Monospace", Font.BOLD, 12),
    displayFontItalicSmall = new Font("Monospace", Font.BOLD | Font.ITALIC, 12),
    lineHeightSmall = 14,
    columnWidthSmall = 100,


    circleRadius = 6,
    arrows3D = false,
    arrowLength = 0.0
  )

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

    circleRadius = 6,
    arrows3D = true,
    arrowLength = 100.0
  )


  val RenderHints = new RenderingHints(
     RenderingHints.KEY_TEXT_ANTIALIASING,
     RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  RenderHints.put(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)

  def arrowPoints(pos: Vec2, dir: Vec2): Seq[Vec2] = {

    val length = 15
    val width = length / 3

    // pos at at base of arrow
    /*
    val tip   = Vec2(pos.x + dir.x * length, pos.y + dir.y * length)
    val left  = Vec2(pos.x - dir.y * width,  pos.y + dir.x * width)
    val right = Vec2(pos.x + dir.y * width,  pos.y - dir.x * width)
    Seq(left, tip, right)
    */

    // pos at tip of arrow
    val tail = Vec2(pos.x - dir.x * length, pos.y - dir.y * length)
    val left  = Vec2(tail.x - dir.y * width, tail.y + dir.x * width)
    val right = Vec2(tail.x + dir.y * width, tail.y - dir.x * width)
    Seq(left, pos, right)

  }


  def arrowPoints3D(pos: Vec3, dir: Vec3, length: Double): Seq[Vec3] = {
    val width = length / 3.0

    val tail = Vec3(pos.x - dir.x * length, pos.y - dir.y * length, pos.z - dir.z * length)
    val left  = Vec3(tail.x - dir.y * width, tail.y + dir.x * width, tail.z)
    val right = Vec3(tail.x + dir.y * width, tail.y - dir.x * width, tail.z)
    Seq(left, pos, right)

  }

}


// Build and apply camera matrices and perspective transformations.

// This code follows the convention that transformation matrices are applied in
// sequence right to left.

// For example, if P is a 4 by m matrix of m 3D points represented by
// homogeneous coordinates, applying the transformations T then U then V
// (which are 4x4 matrices) would be written VUTP.

object View {

  val UnitX = Vec3(1.0, 0.0, 0.0)
  val UnitY = Vec3(0.0, 1.0, 0.0)
  val UnitZ = Vec3(0.0, 0.0, 1.0)

  val Vec3Zero = Vec3(0.0, 0.0, 0.0)

  val Identity3 = Mat33(UnitX, UnitY, UnitZ)
  val IdentityTransformation = transformation(Identity3, Vec3Zero)


  def transformation(rot: Mat33, trans: Vec3): Mat44 = {
     Mat44(
      new Vec4(rot.c0, 0.0),
      new Vec4(rot.c1, 0.0),
      new Vec4(rot.c2, 0.0),
      new Vec4(trans,  1.0)
    )
  }


  // perspective transformation calculations
  // https://en.wikipedia.org/wiki/3D_projection#Perspective_projection

  def cameraTransform(rot: Mat33, pos: Vec3): Mat44 = {

    val translation = transformation(Identity3, Vec3(-pos.x, -pos.y, -pos.z))
    val rotation = transformation(rot, Vec3Zero)
    // interesting to experiment with this
    // translation.mul(rotation)
    rotation.mul(translation)
  }


  def rotationXYZ(theta: Vec3): Mat33 = {
    rotZ(theta.z).mul(rotY(theta.y)).mul(rotX(theta.x))
  }

  def rotationZYX(theta: Vec3): Mat33 = {
    rotX(theta.x).mul(rotY(theta.y)).mul(rotZ(theta.z))
  }


  def rotX(theta: Double): Mat33 = {
    Mat33(
      UnitX,
      Vec3(0.0, math.cos(theta), -math.sin(theta)),
      Vec3(0.0, math.sin(theta),  math.cos(theta)))
  }


  def rotY(theta: Double): Mat33 = {
    Mat33(
      Vec3( math.cos(theta), 0.0, math.sin(theta)),
      UnitY,
      Vec3(-math.sin(theta), 0.0, math.cos(theta)))
  }


  def rotZ(theta: Double): Mat33 = {
    Mat33(
      Vec3(math.cos(theta), -math.sin(theta), 0.0),
      Vec3(math.sin(theta),  math.cos(theta), 0.0),
      UnitZ)
  }


  def perspective(point: Vec3, camTrans: Mat44, viewPos: Vec3): Vec2 = {
    val ph = new Vec4(point, 1.0)
    val pc = camTrans.mul(ph)
    // val p = Vec3(pc.x / pc.w, pc.y / pc.w, pc.z / pc.w)
    val p =  Vec3(pc.x / pc.w, pc.y / pc.w, math.min(pc.z / pc.w, 0.0))

    Vec2(
      viewPos.z * p.x / p.z - viewPos.x,
      viewPos.z * p.y / p.z - viewPos.y
    )
  }

}
