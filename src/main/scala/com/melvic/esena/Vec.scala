package com.melvic.esena

final case class Vec(x: Double, y: Double, z: Double) extends Tuple  {
  val w: Double = 0

  def magnitude: Double =
    math.sqrt(x * x + y * y + z * z + w * w)
}

object Vec {
  val zero: Vec = Vec(0, 0, 0)
}
