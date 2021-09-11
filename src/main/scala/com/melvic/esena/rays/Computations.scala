package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

final case class Computations(
    t: Double,
    obj: Shape,
    point: Point,
    eyeVec: Vec,
    normalVec: Vec,
    inside: Boolean,
)

object Computations {
  def prepare(intersection: Intersection, ray: Ray): Computations = {
    val point = ray.position(intersection.t)
    val normalVec = intersection.obj.normalAt(point)
    val eyeVec = -ray.direction
    val comps = Computations(
      t = intersection.t,
      obj = intersection.obj,
      point = point,
      eyeVec = eyeVec,
      normalVec = normalVec,
      inside = normalVec.dot(eyeVec) < 0
    )
    if (comps.inside)
      comps.copy(normalVec = -comps.normalVec)
    else comps
  }
}
