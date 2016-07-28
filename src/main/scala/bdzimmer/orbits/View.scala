// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Render 3D positions and motions to 2D images.

package bdzimmer.orbits

import scala.collection.immutable.Seq

import java.awt.image.BufferedImage
import java.awt.{Color, Font}


class Viewer(camTrans: Mat44, viewPos: Vec3) {

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
      gr.drawLine(x1, y1, x2, y2)
    }

    xGrid.foreach(x => drawLine(x))
    yGrid.foreach(x => drawLine(x))

  }


  def drawMotion(im: BufferedImage, pos: Points3d, color: Color): Unit = {
    val pos2d = View.perspective(pos, camTrans, viewPos)
    val colInt = color.getRGB
    pos2d.foreach(p => {
      val x = p.x.toInt + im.getWidth / 2
      val y = im.getHeight - (p.y.toInt + im.getHeight / 2)
      if (x >= 0 && x < im.getWidth && y >= 0 && y < im.getHeight) {
        im.setRGB(x, y, colInt)
      }
    })
  }


  def drawPosition(im: BufferedImage, pos: Vec3, name: String, date: String, color: Color): Unit = {
    val pos2d = View.perspective(pos, camTrans, viewPos)
    val gr = im.getGraphics
    gr.setColor(color)

    val x = pos2d.x.toInt + im.getWidth / 2
    val y = im.getHeight - (pos2d.y.toInt + im.getHeight / 2)

    val rad = Viewer.CircleRadius
    gr.fillOval(x - rad, y - rad, rad * 2, rad * 2)
    gr.setFont(Viewer.DisplayFont)
    gr.drawString(name, x + rad, y + Viewer.LineHeight)
    gr.drawString(date, x + rad, y + Viewer.LineHeight * 2)
  }


  def drawPolygon(im: BufferedImage, polygon: Seq[Vec2], color: Color): Unit = {
    val gr = im.getGraphics
    gr.setColor(color)
    gr.fillPolygon(
        polygon.map(v => v.x.toInt + im.getWidth / 2).toArray,
        polygon.map(v => im.getHeight - (v.y.toInt + im.getHeight / 2)).toArray,
        polygon.length)
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
// sequence left to right, rather than right to left.

// For example, if "points" is an Mx4 matrix representing the homogeneous coordinates
// of M 3D points, then to apply the transformation T then U then V (which are 4x4 matrices)
// you would write: points x T x U x V

object View {

  val UnitX = Vec3(1.0, 0.0, 0.0)
  val UnitY = Vec3(0.0, 1.0, 0.0)
  val UnitZ = Vec3(0.0, 0.0, 1.0)
  val Vec3Zero = Vec3(0.0, 0.0, 0.0)

  val Identity3 = Mat33(UnitX, UnitY, UnitZ)
  val IdentityTransformation = buildTransformation(Identity3, Vec3Zero)


  def buildTransformation(rot: Mat33, trans: Vec3): Mat44 = {
    Mat44(
      Vec4(rot.c0.x, rot.c0.y, rot.c0.z, trans.x),
      Vec4(rot.c1.x, rot.c1.y, rot.c1.z, trans.y),
      Vec4(rot.c2.x, rot.c2.y, rot.c2.z, trans.z),
      Vec4(0.0,      0.0,      0.0,      1.0)
    )
  }


  // perspective transformation calculations
  // https://en.wikipedia.org/wiki/3D_projection#Perspective_projection

  def cameraTransform(orient: Vec3, pos: Vec3): Mat44 = {

    val rotX = Mat33(
      UnitX,
      Vec3(0, math.cos(-orient.x), -math.sin(-orient.x)),
      Vec3(0, math.sin(-orient.x),  math.cos(-orient.x))
    )

    val rotY = Mat33(
      Vec3( math.cos(-orient.y), 0, math.sin(-orient.y)),
      UnitY,
      Vec3(-math.sin(-orient.y), 0, math.cos(-orient.y))
    )

    val rotZ = Mat33(
      Vec3(math.cos(-orient.z), -math.sin(-orient.z), 0),
      Vec3(math.sin(-orient.z),  math.cos(-orient.z), 0),
      UnitZ
    )

    val translation = buildTransformation(Identity3, Vec3(-pos.x, -pos.y, -pos.z))
    val rotation = buildTransformation(rotX.mul(rotY).mul(rotZ), Vec3Zero)
    translation.mul(rotation)

  }


  def perspective(coords: Points3d, camTrans: Mat44, viewPos: Vec3): Seq[Vec2] = {
    val coordsHomo = coords.toHomo
    val coordsCam = coordsHomo.mul(camTrans).toEuclidean
    coordsCam.points.map(p => Vec2(
        viewPos.z * p.x / p.z - viewPos.x,
        viewPos.z * p.y / p.z - viewPos.y
    ))
  }


  // TODO: refactor
  def perspective(coord: Vec3, camTrans: Mat44, viewPos: Vec3): Vec2 = {
    perspective(Points3d(List(coord)), camTrans, viewPos)(0)
  }

}
