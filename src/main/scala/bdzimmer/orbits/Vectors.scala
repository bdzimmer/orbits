// Copyright (c) 2016 Ben Zimmer. All rights reserved.

// Just enough pure functional vector math and linear algebra for
// rough calculations and visualizations.

// Uses a type-class-like approach without implicit classes.

package bdzimmer.orbits

import scala.collection.immutable.Seq

trait VectorOps[V] {
  def dot(v1: V, v2: V): Double
  def add(v1: V, v2: V): V
  def sub(v1: V, v2: V): V
  def mul(v1: V, v2: V): V
  def mul(v: V, s: Double): V

  def length(v: V): Double = math.sqrt(dot(v, v))
  def normalize(v: V): V = mul(v, 1.0 / length(v))
}


case class Vec2(x: Double, y: Double)

object Vec2 extends VectorOps[Vec2] {
  def dot(v1: Vec2, v2: Vec2): Double = v1.x * v2.x + v1.y * v2.y
  def add(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x + v2.x, v1.y + v2.y)
  def sub(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x - v2.x, v1.y - v2.y)
  def mul(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x * v2.x, v1.y * v2.x)
  def mul(v1: Vec2, s: Double): Vec2 = Vec2(v1.x * s, v1.y * s)
}


case class Vec3(x: Double, y: Double, z: Double)

object Vec3 extends VectorOps[Vec3] {
  def dot(v1: Vec3, v2: Vec3): Double = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
  def add(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
  def sub(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
  def mul(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z)
  def mul(v1: Vec3, s: Double): Vec3 = Vec3(v1.x * s, v1.y * s, v1.z * s)
}


case class Vec4(x: Double, y: Double, z: Double, w: Double)

object Vec4 extends VectorOps[Vec4] {
  def dot(v1: Vec4, v2: Vec4): Double = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w
  def add(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w)
  def sub(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z, v1.w - v2.w)
  def mul(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z, v1.w * v2.w)
  def mul(v1: Vec4, s: Double): Vec4 = Vec4(v1.x * s, v1.y * s, v1.z * s, v1.w * s)
}


// 3x3 matrix created from 3 column vectors

case class Mat33(c0: Vec3, c1: Vec3, c2: Vec3) {

  val r0 = Vec3(c0.x, c1.x, c2.x)
  val r1 = Vec3(c0.y, c1.y, c2.y)
  val r2 = Vec3(c0.z, c1.z, c2.z)

  def mul(mat: Mat33): Mat33 = {
    Mat33(
        Vec3(Vec3.dot(r0, mat.c0), Vec3.dot(r1, mat.c0), Vec3.dot(r2, mat.c0)),
        Vec3(Vec3.dot(r0, mat.c1), Vec3.dot(r1, mat.c1), Vec3.dot(r2, mat.c1)),
        Vec3(Vec3.dot(r0, mat.c2), Vec3.dot(r1, mat.c2), Vec3.dot(r2, mat.c2))
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
        Vec4(Vec4.dot(r0, mat.c0), Vec4.dot(r1, mat.c0), Vec4.dot(r2, mat.c0), Vec4.dot(r3, mat.c0)),
        Vec4(Vec4.dot(r0, mat.c1), Vec4.dot(r1, mat.c1), Vec4.dot(r2, mat.c1), Vec4.dot(r3, mat.c1)),
        Vec4(Vec4.dot(r0, mat.c2), Vec4.dot(r1, mat.c2), Vec4.dot(r2, mat.c2), Vec4.dot(r3, mat.c2)),
        Vec4(Vec4.dot(r0, mat.c3), Vec4.dot(r1, mat.c3), Vec4.dot(r2, mat.c3), Vec4.dot(r3, mat.c3))
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
      Vec4(Vec4.dot(mat.c0, p), Vec4.dot(mat.c1, p), Vec4.dot(mat.c2, p), Vec4.dot(mat.c3, p))
    }))
  }

  def toEuclidean(): Points3d = {
    Points3d(points.map(p => Vec3(p.x / p.w, p.y / p.w, p.z / p.w)))
  }
}
