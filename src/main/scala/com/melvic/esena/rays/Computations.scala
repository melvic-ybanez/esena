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
    overPoint: Point, // lifts the point above the surface
    underPoint: Point, // lifts the point below the surface
    reflectVec: Vec,
    refractive: RefractiveIndices,
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
    // point of intersection to lie beneath the surface of the object, which
    // can make the object shadow itself.
    val overPoint = point + normalVec * MathUtils.Epsilon

    // Just like over point, but it goes the opposite direction
    // (i.e. it lifts the point below the surface rather than above it)
    val underPoint = point - normalVec * MathUtils.Epsilon

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
      refractive = RefractiveIndices(n1, n2),
      underPoint = underPoint
    )
    if (comps.inside) {
      val negatedNormal = -comps.normalVec
      comps.copy(normalVec = negatedNormal, reflectVec = reflectVec(negatedNormal))
    } else comps
  }

  def computeRefractiveIndices(hit: Intersection, intersections: Intersections): (Double, Double) = {
    var (n1, n2) = (Refraction.index.Default, Refraction.index.Default)

    // objects entered but not yet exited
    val containers: ArrayBuffer[Shape] = ArrayBuffer.empty

    // Update n1 and n2 based on the current state of `containers`
    // when hit == intersection
    for (intersection <- intersections) {
      // if `containers` is still not populated at this point,
      // then there is no containing object
      if (intersection == hit && containers.nonEmpty)
        n1 = containers.last.material.refractiveIndex

      if (containers.contains(intersection.obj)) {
        // the intersection must be exiting the object, we should
        // remove it from the list
        containers -= intersection.obj
      } else containers.addOne(intersection.obj)

      if (intersection == hit && containers.nonEmpty)
        n2 = containers.last.material.refractiveIndex
    }

    (n1, n2)
  }
}
