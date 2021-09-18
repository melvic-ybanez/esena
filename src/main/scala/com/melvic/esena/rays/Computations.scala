package com.melvic.esena.rays

import com.melvic.esena.MathUtils
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.reflections.Refraction
import com.melvic.esena.reflections.Refraction.RefractiveIndices
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

import scala.collection.mutable.ArrayBuffer

final case class Computations(
    t: Double,
    obj: Shape,
    point: Point,
    eyeVec: Vec,
    normalVec: Vec,
    inside: Boolean,
    overPoint: Point,
    reflectVec: Vec,
    refractive: RefractiveIndices
)

object Computations {
  def prepare(intersection: Intersection, ray: Ray, intersections: Intersections = Vector.empty): Computations = {
    val xs = if (intersections.isEmpty) Vector(intersection) else intersections

    val point     = ray.position(intersection.t)
    val normalVec = intersection.obj.normalAt(point)
    val eyeVec    = -ray.direction

    // This is to prevent the acne effect.
    // It moves the point a tiny bit to the direction of the normal. We use
    // this to prevent some floating point rounding errors from making the
    // point of intersection to lie beneath the surface of the object.
    val overPoint = point + normalVec * MathUtils.Epsilon

    def reflectVec(normal: Vec): Vec =
      ray.direction.reflect(normal)

    val (n1, n2) = computeRefractiveIndices(intersection, xs)

    val comps = Computations(
      t = intersection.t,
      obj = intersection.obj,
      point = point,
      eyeVec = eyeVec,
      normalVec = normalVec,
      inside = normalVec.dot(eyeVec) < 0,
      overPoint = overPoint,
      // reflect the ray's direction around the object's normal vector
      reflectVec = reflectVec(normalVec),
      refractive = RefractiveIndices(n1, n2)
    )
    if (comps.inside) {
      val negatedNormal = -comps.normalVec
      comps.copy(normalVec = negatedNormal, reflectVec = reflectVec(negatedNormal))
    } else comps
  }

  def computeRefractiveIndices(hit: Intersection, xs: Intersections): (Double, Double) = {
    var (n1, n2) = (Refraction.index.Default, Refraction.index.Default)
    val containers: ArrayBuffer[Shape] = ArrayBuffer.empty

    for (i <- xs) {
      if (i == hit && containers.nonEmpty)
        n1 = containers.last.material.refractiveIndex

      if (containers.contains(i.obj))
        containers.remove(containers.indexOf(i.obj))
      else containers.addOne(i.obj)

      if (i == hit && containers.nonEmpty)
        n2 = containers.last.material.refractiveIndex
    }

    (n1, n2)
  }
}
