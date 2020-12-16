// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Render 3D positions and motions to 2D images.

package bdzimmer.orbits

import scala.collection.immutable.Seq
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Font, FontMetrics, Graphics2D, RenderingHints, Stroke}


class Viewer(val camTrans: Mat44, val viewPos: Vec3, val settings: ViewerSettings) {

  def drawGrid(
      im: BufferedImage,
      gridLim: Double,
      ticks: Int,
      transform: Option[Mat44],
      color: Color): Unit = {

    val spacing = gridLim / ticks
    val xRange = (-ticks to ticks).map(x => x * spacing)
    val yRange = xRange

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)
    gr.setStroke(settings.stroke)

    def drawSegment(pt1: Vec3, pt2: Vec3): Unit = {
      val pt1T = transform.map(Transformations.transform(_, pt1)).getOrElse(pt1)
      val pt2T = transform.map(Transformations.transform(_, pt2)).getOrElse(pt2)

      val start = View.perspective(pt1T, camTrans, viewPos)
      val end   = View.perspective(pt2T, camTrans, viewPos)
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
      lines: Boolean,
      verticals: Boolean,
      adjustAlpha: Boolean = true): Unit = {

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
      gr.setStroke(settings.stroke)
      if (pos2d.length > 1) {
        pos2d.zipWithIndex.dropRight(1).foreach({case (p1, idx) => {

          if (adjustAlpha) {
            // experiment with transparency
            val fraction = 1.0 * idx / pos2d.length
            val alpha = 255.0 * fraction
            // val alpha = if (1.0 * idx / pos2d.length > 0.5) { 255.0 } else { 127.0 }
            val artsyColor = new Color(
              color.getRed, color.getGreen, color.getBlue, alpha.toInt)
            gr.setColor(artsyColor)
          }

          val (x1, y1) = Viewer.cvtPos(im, p1.x.toInt, p1.y.toInt)  // TODO: experiment with round
          val p2 = pos2d(idx + 1)
          val (x2, y2) = Viewer.cvtPos(im, p2.x.toInt, p2.y.toInt)
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

          val (x0, y0) = Viewer.cvtPos(im, p0.x.toInt, p0.y.toInt)
          val (x0_b, y0_b) = Viewer.cvtPos(im, p0_b.x.toInt, p0_b.y.toInt)
          val (x1, y1) = Viewer.cvtPos(im, p1.x.toInt, p1.y.toInt)
          val (x1_b, y1_b) = Viewer.cvtPos(im, p1_b.x.toInt, p1_b.y.toInt)

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


  def drawLine(im: BufferedImage, v1: Vec3, v2: Vec3, color: Color): Unit = {
    val p1 = View.perspective(v1, camTrans, viewPos)
    val p2 = View.perspective(v2, camTrans, viewPos)
    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)
    gr.setStroke(settings.stroke)

    val (x1, y1) = Viewer.cvtPos(im, p1.x.toInt, p1.y.toInt)
    val (x2, y2) = Viewer.cvtPos(im, p2.x.toInt, p2.y.toInt)
    if (x1.abs < Viewer.DrawMax && y1.abs < Viewer.DrawMax && x2.abs < Viewer.DrawMax && y2.abs < Viewer.DrawMax) {
      gr.drawLine(x1, y1, x2, y2)
    }

  }


  def drawPosition(
      im: BufferedImage, pos: Vec3, name: String, desc: String,
      color: Color, fill: Boolean = true): Unit = {

    val pos2d = View.perspective(pos, camTrans, viewPos)
    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)

    // TODO: use cvtPos
    val x = pos2d.x.toInt + im.getWidth / 2
    val y = im.getHeight - (pos2d.y.toInt + im.getHeight / 2)

    val rad = settings.circleRadius
    if (fill) {
      gr.fillOval(x - rad, y - rad, rad * 2, rad * 2)
    } else {
      gr.drawOval(x - rad, y - rad, rad * 2, rad * 2)
    }

    gr.setFont(settings.displayFont)
    if (name.length > 0) {
      gr.drawString(name, x + rad, y + settings.lineHeight)
    }
    if (desc.length > 0) {
      gr.drawString(desc, x + rad, y + settings.lineHeight * 2)
    }
  }


  def drawLabel(im: BufferedImage, label: String, desc: String, pos: Vec3, color: Color): Unit = {

    val pos2d = View.perspective(pos, camTrans, viewPos)
    val (x, y) = Viewer.cvtPos(im, pos2d.x.toInt, pos2d.y.toInt)

    val gr = im.getGraphics.asInstanceOf[Graphics2D]
    gr.setRenderingHints(Viewer.RenderHints)
    gr.setColor(color)

    gr.setFont(settings.displayFont)
    if (label.length > 0) {
      gr.drawString(label, x, y + settings.lineHeight)
    }
    if (desc.length > 0) {
      gr.drawString(desc, x, y + settings.lineHeight * 2)
    }

  }


  def drawPolygon(im: BufferedImage, polygon: Seq[Vec2], color: Color, fill: Boolean): Unit = {
    val ptsShifted = polygon.map(p => Viewer.cvtPos(im, p.x.toInt, p.y.toInt))
    if (!ptsShifted.exists(p => p._1.abs > Viewer.DrawMax || p._2.abs > Viewer.DrawMax)) {
      val xs = ptsShifted.map(_._1).toArray
      val ys = ptsShifted.map(_._2).toArray
      val gr = im.getGraphics.asInstanceOf[Graphics2D]
      gr.setRenderingHints(Viewer.RenderHints)
      gr.setColor(color)
      if (fill) {
        gr.fillPolygon(xs, ys, polygon.length)
      } else {
        gr.drawPolygon(xs, ys, polygon.length)
      }
    }
  }


  def drawArrow(im: BufferedImage, os: OrbitalState, color: Color): Unit = {
    if (!settings.arrows3D) {
      val position = View.perspective(os.position, camTrans, viewPos)
      val direction = Vec2.normalize(Vec2.sub(
        View.perspective(Vec3.add(os.position, Vec3.normalize(os.velocity)), camTrans, viewPos),
        position))
      val arrowPoints = Viewer.arrowPoints(position, direction)
      drawPolygon(im, arrowPoints, color, true)
    } else {

      // val arrowScale = settings.arrowLength / viewPos.z
      val arrowPointCamera = Transformations.transform(camTrans, os.position)
      val arrowCameraDist = Vec3.length(arrowPointCamera)
      val arrowScale = settings.arrowLength / viewPos.z * arrowCameraDist * 0.15

      val arrowPoints3d = Viewer.arrowPoints3D(
        os.position, Vec3.normalize(os.velocity), arrowScale)
      val arrowPoints = arrowPoints3d.map(x => View.perspective(x, camTrans, viewPos))
      drawPolygon(im, arrowPoints, color, true)
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
      drawPolygon(im, tri0.map(View.perspective(_, camTrans, viewPos)), color, true)
      val tri1 = Seq(r0pts(idx), r1pts(idx - 1), r0pts(idx - 1))
      drawPolygon(im, tri1.map(View.perspective(_, camTrans, viewPos)), color, true)
    }
  }


  def drawSphere(
      im: BufferedImage, transform: Mat44, scale: Vec3, color: Color,
      backfaceCulling: Boolean): Unit = {

    if (backfaceCulling) {
      // transform sphere points and draw individual lines

      val camTranslation = Vec3(camTrans.c3.x, camTrans.c3.y, camTrans.c3.z)  // technically negative of camera translation
      val transTranslation = Vec3(transform.c3.x, transform.c3.y, transform.c3.z)
      val sunToPlanet = Vec3.normalize(transTranslation)

      for (circle <- Primitives.DefaultSphere) {

        val ct = circle.map(x => Transformations.transform(transform, Vec3.emul(x, scale)))
        val linePoints = (ct :+ ct.head).sliding(2)

        for (line <- linePoints) {
          val pt0 = line(0)  // unsafe but whatever
          val pt1 = line(1)  //

          // examine the average of the two points of the line
          // with only rotations applied.

          val ptMean = Vec3.mul(Vec3.add(pt0, pt1), 0.5)

          // undo first translation
          val ptUndoTranslation = Vec3.sub(ptMean, transTranslation)

          // apply the camera rotation only
          val ptFinal = Vec3.normalize(
            Vec3.sub(Transformations.transform(camTrans, ptUndoTranslation), camTranslation))

          // if it points in the same direction as camera coordinate system Z axis
          // (unit Z), show it
          if (Vec3.dot(ptFinal, Transformations.UnitZ) > 0.0) {

            // shade all the same color
            // drawLine(im, pt0, pt1, color)

            // shade with sunlight!

            val shade = 0.0 - Vec3.dot(sunToPlanet, Vec3.normalize(ptUndoTranslation))
            if (shade > 0.0) {
              val shadeColor = new Color(
                (color.getRed * shade).toInt,
                (color.getGreen * shade).toInt,
                (color.getBlue * shade).toInt)
              drawLine(im, pt0, pt1, shadeColor)
            }

          }
        }
      }


    } else {
      // transform sphere points and draw
      for (circle <- Primitives.DefaultSphere) {
        val ct = circle.map(x => Transformations.transform(transform, Vec3.emul(x, scale)))
        drawPolygon(im, ct.map(View.perspective(_, camTrans, viewPos)), color, false)
      }
    }
  }

}


object Viewer {

  val DrawMax = 32768

  val RenderHints = new RenderingHints(
     RenderingHints.KEY_TEXT_ANTIALIASING,
     RenderingHints.VALUE_TEXT_ANTIALIAS_ON)
  RenderHints.put(
      RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON)


  def cvtPos(im: BufferedImage, x: Int, y: Int): (Int, Int) = {
    (x + im.getWidth / 2, im.getHeight - (y + im.getHeight / 2))
  }


  // TODO: something like this that takes the size of the object into account
  // estimate whether object at coordinates should be drawn
  def inView(im: BufferedImage, x: Int, y: Int): Boolean = {
    x > -im.getWidth() && x < 2 * im.getWidth && y > -im.getHeight && y < 2 * im.getHeight
  }


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

  // perspective transformation calculations
  // https://en.wikipedia.org/wiki/3D_projection#Perspective_projection

  def cameraTransform(rot: Mat33, pos: Vec3): Mat44 = {

    val translation = Transformations.transformation(Transformations.Identity3, Vec3(-pos.x, -pos.y, -pos.z))
    val rotation = Transformations.transformation(rot, Transformations.Vec3Zero)
    // interesting to experiment with this
    // translation.mul(rotation)
    rotation.mul(translation)
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
