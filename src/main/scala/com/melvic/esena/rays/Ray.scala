package com.melvic.esena.rays

import com.melvic.esena.tuples.{Point, Tuple, Vec}

final case class Ray(origin: Point, direction: Vec) {
  def position(t: Double): Tuple =
    origin + direction * t

  def apply(t: Double): Tuple = position(t)
}
