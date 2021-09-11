package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

final case class Computation(
    t: Double,
    obj: Shape,
    point: Point,
    eyeVec: Vec,
    normalVec: Vec
)

object Computation {
  def prepare(intersection: Intersection, ray: Ray): Computation = {
    val point = ray.position(intersection.t)
    Computation(
      t = intersection.t,
      obj = intersection.obj,
      point = point,
      eyeVec = -ray.direction,
      normalVec = intersection.obj.normalAt(point)
    )
  }
}
