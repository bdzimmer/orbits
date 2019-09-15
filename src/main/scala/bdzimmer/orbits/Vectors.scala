// Copyright (c) 2017 Ben Zimmer. All rights reserved.

// Just enough pure functional vector math and linear algebra for
// rough calculations and visualizations.

// Uses a type-class-like approach without implicit classes.

package bdzimmer.orbits

import scala.collection.immutable.Seq


trait VectorOps[V] {
  def add(v1: V, v2: V): V       // vector addition
  def mul(v: V, s: Double): V    // scalar multiplication

  def dot(v1: V, v2: V): Double  // dot product

  def sub(v1: V, v2: V): V       // vector subtraction
  def emul(v1: V, v2: V): V      // elementwise multiplication

  def length(v: V): Double = math.sqrt(dot(v, v))
  def normalize(v: V): V = mul(v, 1.0 / length(v))
}


case class Vec2(x: Double, y: Double)

object Vec2 extends VectorOps[Vec2] {
  def add(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x + v2.x, v1.y + v2.y)
  def mul(v1: Vec2, s: Double): Vec2 = Vec2(v1.x * s, v1.y * s)
  def dot(v1: Vec2, v2: Vec2): Double = v1.x * v2.x + v1.y * v2.y
  def sub(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x - v2.x, v1.y - v2.y)
  def emul(v1: Vec2, v2: Vec2): Vec2 = Vec2(v1.x * v2.x, v1.y * v2.x)
}


case class Vec3(x: Double, y: Double, z: Double)

object Vec3 extends VectorOps[Vec3] {
  def add(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
  def mul(v1: Vec3, s: Double): Vec3 = Vec3(v1.x * s, v1.y * s, v1.z * s)
  def dot(v1: Vec3, v2: Vec3): Double = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z
  def sub(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
  def emul(v1: Vec3, v2: Vec3): Vec3 = Vec3(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z)

  def cross(a: Vec3, b: Vec3): Vec3 = Vec3(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
  )
}


case class Vec4(x: Double, y: Double, z: Double, w: Double) {
  def this(v: Vec3, w: Double) = this(v.x, v.y, v.z, w)
}

object Vec4 extends VectorOps[Vec4] {
  def add(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z, v1.w + v2.w)
  def mul(v1: Vec4, s: Double): Vec4 = Vec4(v1.x * s, v1.y * s, v1.z * s, v1.w * s)
  def dot(v1: Vec4, v2: Vec4): Double = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w
  def sub(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z, v1.w - v2.w)
  def emul(v1: Vec4, v2: Vec4): Vec4 = Vec4(v1.x * v2.x, v1.y * v2.y, v1.z * v2.z, v1.w * v2.w)
}


// 3x3 matrix created from 3 column vectors

case class Mat33(c0: Vec3, c1: Vec3, c2: Vec3) {

  val r0 = Vec3(c0.x, c1.x, c2.x)
  val r1 = Vec3(c0.y, c1.y, c2.y)
  val r2 = Vec3(c0.z, c1.z, c2.z)

  def mul(vec: Vec3): Vec3 = {
    Vec3(Vec3.dot(r0, vec), Vec3.dot(r1, vec), Vec3.dot(r2, vec))
  }

  def mul(mat: Mat33): Mat33 = {
     Mat33(mul(mat.c0), mul(mat.c1), mul(mat.c2))
  }

}


// 4x4 matrix created from 4 column vectors

case class Mat44(c0: Vec4, c1: Vec4, c2: Vec4, c3: Vec4) {

  val r0 = Vec4(c0.x, c1.x, c2.x, c3.x)
  val r1 = Vec4(c0.y, c1.y, c2.y, c3.y)
  val r2 = Vec4(c0.z, c1.z, c2.z, c3.z)
  val r3 = Vec4(c0.w, c1.w, c2.w, c3.w)

  def mul(vec: Vec4): Vec4 = {
    Vec4(Vec4.dot(r0, vec), Vec4.dot(r1, vec), Vec4.dot(r2, vec), Vec4.dot(r3, vec))
  }

  def mul(mat: Mat44): Mat44 = {
    Mat44(mul(mat.c0), mul(mat.c1), mul(mat.c2), mul(mat.c3))
  }

  def mul(pts: Seq[Vec4]): Seq[Vec4] = {
    pts.map(p => mul(p))
  }

}
