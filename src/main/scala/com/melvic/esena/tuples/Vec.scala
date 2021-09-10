package com.melvic.esena.tuples

final case class Vec(x: Double, y: Double, z: Double) extends Tuple  {
  val w: Double = 0

  val isUnit: Boolean =
    magnitude == 1

  /**
   * Transforms the vector into a unit vector
   */
  def normalize: Vec =
    Vec(x / magnitude, y / magnitude, z / magnitude)

  def cross(that: Vec): Vec =
    Vec(
      y * that.z - z * that.y,
      z * that.x - x * that.z,
      x * that.y - y * that.x
    )

  def reflect(normal: Vec): Vec =
    (this - normal * this.dot(normal) * 2.0).toVec
}

object Vec {
  val Zero: Vec = Vec(0, 0, 0)
}
