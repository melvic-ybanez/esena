package com.melvic.esena.rays

import com.melvic.esena.Real
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.{Point, Tuple, Vec}

final case class Ray(origin: Point, direction: Vec) {
  def position(t: Real): Point =
    (origin + direction * t).toPoint

  def apply(t: Real): Tuple = position(t)

  def transform(transformation: Matrix): Ray =
    Ray((transformation * origin).toPoint, transformation * direction)
}
