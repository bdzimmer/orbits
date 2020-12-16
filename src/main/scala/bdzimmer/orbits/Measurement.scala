// Copyright (c) 2020 Ben Zimmer. All rights reserved.
// Data structures for various measurements that can be rendered in the 3D view.

package bdzimmer.orbits

sealed abstract class Measurement {
  val dispOffset: (Int, Int)
}

// the dumbest way to do this
case class LookupDistance(
  val fst: String,
  val snd: String,
  val dispOffset: (Int, Int)
) extends Measurement

case class LookupRelativeVelocity(
  val fst: String,
  val snd: String,
  val dispOffset: (Int, Int)
) extends Measurement



object Measurement {
  // TODO:
}
