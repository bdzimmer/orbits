// Copyright (c) 2019 Ben Zimmer. All rights reserved.

package bdzimmer.orbits

import scala.collection.immutable.Seq


object Primitives {

  val DefaultSphere = sphere(16)

  def sphere(resolution: Int): Seq[Seq[Vec3]] = {
    val thetas = 0.0 to 2.0 * math.Pi by (2.0 * math.Pi / resolution)
    val phis = 0.0 to 2.0 * math.Pi by (2.0 * math.Pi / resolution)

//    println(thetas.length)
//    println(phis.length)
//    println(thetas.map(_ * 180.0 / math.Pi))
//    println(phis.map(_ * 180.0 / math.Pi))
//    println(phis.dropRight(resolution / 2).drop(1).dropRight(1))

    // lines of latitude
    val lat = phis.dropRight(resolution / 2).drop(1).map(phi => thetas.map(theta =>
      Vec3(math.cos(theta) * math.sin(phi), math.sin(theta) * math.sin(phi), math.cos(phi))))

    // lines of longitude
    val long = thetas.dropRight(resolution / 2).map(theta => phis.map(phi =>
      Vec3(math.cos(theta) * math.sin(phi), math.sin(theta) * math.sin(phi), math.cos(phi))))

    lat ++ long
  }

}
