// Copyright (c) 2016 Ben Zimmer. All rights reserved.

package bdzimmer.orbits


case class Vec3(x: Double, y: Double, z: Double) {
  def length(): Double = {
    math.sqrt(x * x + y * y + z * z)
  }
}