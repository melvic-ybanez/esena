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

  def cross(that: Vec): Vec =
    Vec(
      y * that.z - z * that.y,
      z * that.x - x * that.z,
      x * that.y - y * that.x
    )
}

object Vec {
  val zero: Vec = Vec(0, 0, 0)
}
