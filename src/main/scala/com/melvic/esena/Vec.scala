package com.melvic.esena

final case class Vec(x: Double, y: Double, z: Double) extends Tuple  {
  val w: Double = 0
}

object Vec {
  val zero: Vec = Vec(0, 0, 0)
}
