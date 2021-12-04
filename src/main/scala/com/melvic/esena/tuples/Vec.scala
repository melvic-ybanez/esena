package com.melvic.esena.tuples

import com.melvic.esena.Real

final case class Vec(x: Real, y: Real, z: Real) extends Tuple {
  val w: Real = 0

  val isUnit: Boolean =
    magnitude == 1

  /**
    * Transforms the vector into a unit vector
    */
  def normalize: Vec =
    if (magnitude == 0) this
    else Vec(x / magnitude, y / magnitude, z / magnitude)

  /**
    * Computes for the third vector that is perpendicular
    * to the other two
    */
  def cross(that: Vec): Vec =
    Vec(
      y * that.z - z * that.y,
      z * that.x - x * that.z,
      x * that.y - y * that.x
    )

  def reflect(normal: Vec): Vec =
    this - normal * 2.0 * this.dot(normal)
}

object Vec {
  val Zero: Vec = Vec(0, 0, 0)
}
