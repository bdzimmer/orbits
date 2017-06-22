// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Render 3D positions and motions to 2D images.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import java.awt.image.BufferedImage
import java.awt.{Color, Font}


class Viewer(val camTrans: Mat44, val viewPos: Vec3) {

  def drawGrid(im: BufferedImage, gridLim: Int, color: Color): Unit = {

    val xGrid = (-gridLim.toDouble to gridLim by 1.0).map(x => (Vec3(x, -gridLim, 0), Vec3(x, gridLim, 0)))
    val yGrid = (-gridLim.toDouble to gridLim by 1.0).map(x => (Vec3(-gridLim, x, 0), Vec3(gridLim, x, 0)))

    val gr = im.getGraphics
    gr.setColor(color)

    def drawLine(pts: (Vec3, Vec3)): Unit = {
      val start = View.perspective(pts._1, camTrans, viewPos)
      val end   = View.perspective(pts._2, camTrans, viewPos)
      val x1 = start.x.toInt + im.getWidth / 2
      val y1 = im.getHeight - (start.y.toInt + im.getHeight / 2)
      val x2 = end.x.toInt + im.getWidth / 2
      val y2 = im.getHeight - (end.y.toInt + im.getHeight / 2)
      // println(x1 + " " + y1 + " " + x2 + " " + y2)
      gr.drawLine(x1, y1, x2, y2)
    }

    xGrid.foreach(x => drawLine(x))
    yGrid.foreach(x => drawLine(x))

  }


  def drawMotion(im: BufferedImage, pos: Seq[Vec3], color: Color): Unit = {
    val pos2d = pos.map(p => View.perspective(p, camTrans, viewPos))
    val colInt = color.getRGB
    pos2d.foreach(p => {
      val x = p.x.toInt + im.getWidth / 2
      val y = im.getHeight - (p.y.toInt + im.getHeight / 2)
      if (x >= 0 && x < im.getWidth && y >= 0 && y < im.getHeight) {
        im.setRGB(x, y, colInt)
      }
    })
  }


  def drawPosition(im: BufferedImage, pos: Vec3, name: String, desc: String, color: Color): Unit = {
    val pos2d = View.perspective(pos, camTrans, viewPos)
    val gr = im.getGraphics
    gr.setColor(color)

    val x = pos2d.x.toInt + im.getWidth / 2
    val y = im.getHeight - (pos2d.y.toInt + im.getHeight / 2)

    val rad = Viewer.CircleRadius
    gr.fillOval(x - rad, y - rad, rad * 2, rad * 2)
    gr.setFont(Viewer.DisplayFont)
    gr.drawString(name, x + rad, y + Viewer.LineHeight)
    gr.drawString(desc, x + rad, y + Viewer.LineHeight * 2)
  }


  def drawPolygon(im: BufferedImage, polygon: Seq[Vec2], color: Color): Unit = {
    val gr = im.getGraphics
    gr.setColor(color)
    gr.fillPolygon(
        polygon.map(v => v.x.toInt + im.getWidth / 2).toArray,
        polygon.map(v => im.getHeight - (v.y.toInt + im.getHeight / 2)).toArray,
        polygon.length)
  }

  def drawArrow(im: BufferedImage, os: OrbitalState, color: Color): Unit = {
    val position = View.perspective(os.position, camTrans, viewPos)
    val direction = Vec2.normalize(Vec2.sub(
        View.perspective(Vec3.add(os.position, os.velocity), camTrans, viewPos),
        position))
    val arrowPoints = Viewer.arrowPoints(position, direction)
    drawPolygon(im, arrowPoints, color)
  }

}



object Viewer {

  val DisplayFont = new Font("Monospace", Font.BOLD, 12)
  val LineHeight = 14
  val CircleRadius = 6

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
    val tip   = Vec2(dir.x * length, dir.y * length)
    val left  = Vec2(pos.x - dir.y * width - tip.x, pos.y + dir.x * width - tip.y)
    val right = Vec2(pos.x + dir.y * width - tip.x, pos.y - dir.x * width - tip.y)
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
    val p = Vec3(pc.x / pc.w, pc.y / pc.w, pc.z / pc.w)
    Vec2(
      viewPos.z * p.x / p.z - viewPos.x,
      viewPos.z * p.y / p.z - viewPos.y
    )
  }

}
