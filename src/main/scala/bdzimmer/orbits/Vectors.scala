// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Just enough pure functional vector math and linear algebra for
// rough calculations and visualizations.

package bdzimmer.orbits

import scala.collection.immutable.Seq


case class Vec2(x: Double, y: Double) {
  def length(): Double =  math.sqrt(x * x + y * y)
  def dot(v: Vec3): Double =  x * v.x + y * v.y

  def mul(s: Double): Vec2 = Vec2(x * s, y * s)
  def normalize(): Vec2 = mul(1.0 / length)
}


case class Vec3(x: Double, y: Double, z: Double) {

  def length(): Double = math.sqrt(dot(this))
  def dot(v: Vec3): Double =  x * v.x + y * v.y + z * v.z

  def sub(v: Vec3): Vec3 = Vec3(x - v.x, y - v.y, z - v.z)
  def add(v: Vec3): Vec3 = Vec3(x + v.x, y + v.y, z + v.z)
  def mul(v: Vec3):   Vec3 = Vec3(x * v.x, y * v.y, z * v.z)
  def mul(s: Double): Vec3 = Vec3(x * s,   y * s,   z * s)
  def normalize(): Vec3 = mul(1.0 / length)
}


case class Vec4(x: Double, y: Double, z: Double, w: Double) {
  def length(): Double = math.sqrt(x * x + y * y + z * z + w * w)
  def dot(v: Vec4): Double = x * v.x + y * v.y + z * v.z + w * v.w
}


// 3x3 matrix created from 3 column vectors

case class Mat33(c0: Vec3, c1: Vec3, c2: Vec3) {

  val r0 = Vec3(c0.x, c1.x, c2.x)
  val r1 = Vec3(c0.y, c1.y, c2.y)
  val r2 = Vec3(c0.z, c1.z, c2.z)

  def mul(mat: Mat33): Mat33 = {
    Mat33(
        Vec3(r0.dot(mat.c0), r1.dot(mat.c0), r2.dot(mat.c0)),
        Vec3(r0.dot(mat.c1), r1.dot(mat.c1), r2.dot(mat.c1)),
        Vec3(r0.dot(mat.c2), r1.dot(mat.c2), r2.dot(mat.c2))
    )
  }
}


// 4x4 matrix created from 4 column vectors

case class Mat44(c0: Vec4, c1: Vec4, c2: Vec4, c3: Vec4) {

  val r0 = Vec4(c0.x, c1.x, c2.x, c3.x)
  val r1 = Vec4(c0.y, c1.y, c2.y, c3.y)
  val r2 = Vec4(c0.z, c1.z, c2.z, c3.z)
  val r3 = Vec4(c0.w, c1.w, c2.w, c3.w)

  def mul(mat: Mat44): Mat44 = {
    Mat44(
        Vec4(r0.dot(mat.c0), r1.dot(mat.c0), r2.dot(mat.c0), r3.dot(mat.c0)),
        Vec4(r0.dot(mat.c1), r1.dot(mat.c1), r2.dot(mat.c1), r3.dot(mat.c1)),
        Vec4(r0.dot(mat.c2), r1.dot(mat.c2), r2.dot(mat.c2), r3.dot(mat.c2)),
        Vec4(r0.dot(mat.c3), r1.dot(mat.c3), r2.dot(mat.c3), r3.dot(mat.c3))
    )
  }
}


// collections of points represented as lists of row vectors

case class Points2d(points: Seq[Vec2])


case class Points3d(points: Seq[Vec3]) {
  def toHomo(): Points3dHomo = {
    Points3dHomo(points.map(p => Vec4(p.x, p.y, p.z, 1.0)))
  }
}


case class Points3dHomo(points: Seq[Vec4]) {

  def mul(mat: Mat44): Points3dHomo = {
    Points3dHomo(points.map(p => {
      Vec4(mat.c0.dot(p), mat.c1.dot(p), mat.c2.dot(p), mat.c3.dot(p))
    }))
  }

  def toEuclidean(): Points3d = {
    Points3d(points.map(p => Vec3(p.x / p.w, p.y / p.w, p.z / p.w)))
  }
}
