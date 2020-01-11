// Copyright (c) 2019 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

object Transformations {

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


  def transform(mat: Mat44, vec: Vec3): Vec3 = {
    val vecH = new Vec4(vec, 1.0)
    val pH = mat.mul(vecH)
    Vec3(pH.x / pH.w, pH.y / pH.w, pH.z / pH.w)
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
      Vec3(0.0,  math.cos(theta), math.sin(theta)),
      Vec3(0.0, -math.sin(theta),  math.cos(theta)))
  }


  def rotY(theta: Double): Mat33 = {
    Mat33(
      Vec3(math.cos(theta), 0.0, -math.sin(theta)),
      UnitY,
      Vec3(math.sin(theta), 0.0,  math.cos(theta)))
  }


  def rotZ(theta: Double): Mat33 = {
    Mat33(
      Vec3(math.cos(theta),  math.sin(theta), 0.0),
      Vec3(-math.sin(theta), math.cos(theta), 0.0),
      UnitZ)
  }







}
