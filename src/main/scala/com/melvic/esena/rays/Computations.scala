package com.melvic.esena.rays

import com.melvic.esena.MathUtils
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

final case class Computations(
    t: Double,
    obj: Shape,
    point: Point,
    eyeVec: Vec,
    normalVec: Vec,
    inside: Boolean,
    overPoint: Point,
)

object Computations {
  def prepare(intersection: Intersection, ray: Ray): Computations = {
    val point = ray.position(intersection.t)
    val normalVec = intersection.obj.normalAt(point)
    val eyeVec = -ray.direction

    // This is to prevent the acne effect.
    // It moves the point a tiny bit to the direction of the normal. We use
    // this to prevent some floating point rounding errors from making the
    // point of intersection to lie beneath the surface of the object.
    val overPoint = point + normalVec * MathUtils.Epsilon

    val comps = Computations(
      t = intersection.t,
      obj = intersection.obj,
      point = point,
      eyeVec = eyeVec,
      normalVec = normalVec,
      inside = normalVec.dot(eyeVec) < 0,
      overPoint = overPoint
    )
    if (comps.inside)
      comps.copy(normalVec = -comps.normalVec)
    else comps
  }
}
