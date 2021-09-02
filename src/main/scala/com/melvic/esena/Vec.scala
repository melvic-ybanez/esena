package com.melvic.esena

final case class Vec(x: Double, y: Double, z: Double) extends Tuple  {
  val w: Double = 0

  val isUnit: Boolean =
    magnitude == 1

  /**
   * Transforms the vector into a unit vector
   */
  def normalize: Tuple =
    Tuple(x / magnitude, y / magnitude, z / magnitude, w / magnitude)
}

object Vec {
  val zero: Vec = Vec(0, 0, 0)
}
