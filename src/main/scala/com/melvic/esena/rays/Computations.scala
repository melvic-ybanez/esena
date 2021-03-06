package com.melvic.esena.rays

import com.melvic.esena.{MathUtils, Real}
import com.melvic.esena.dielectrics.Refraction
import com.melvic.esena.dielectrics.Refraction.RefractiveIndices
import com.melvic.esena.rays.Computations.LiftingPoints
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.shapes.{Shape, TestShape}
import com.melvic.esena.tuples.{Point, Vec}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

final case class Computations(
    t: Real,
    obj: Shape,
    point: Point,
    eyeVec: Vec,
    normalVec: Vec,
    inside: Boolean,
    liftingPoints: LiftingPoints,
    reflectVec: Vec,
    refractive: RefractiveIndices
) {
  def n1 = refractive.n1

  def n2 = refractive.n2

  def overPoint: Point = liftingPoints.overPoint

  def underPoint: Point = liftingPoints.underPoint
}

object Computations {
  case class LiftingPoints(
      overPoint: Point, // lifts the point above the surface
      underPoint: Point // lifts the point below the surface
  )

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
      liftingPoints = LiftingPoints(overPoint, underPoint),
      // reflect the ray's direction around the object's normal vector
      reflectVec = reflectVec(normalVec),
      refractive = RefractiveIndices(n1, n2)
    )
    if (comps.inside) {
      val negatedNormal = -comps.normalVec
      comps.copy(normalVec = negatedNormal, reflectVec = reflectVec(negatedNormal))
    } else comps
  }

  def computeRefractiveIndices(hit: Intersection, intersections: Intersections): (Real, Real) = {
    // objects entered but not yet exited
    val containers: ArrayBuffer[Shape] = ArrayBuffer.empty

    // Update n1 and n2 based on the current state of `containers`
    // when hit == intersection
    @tailrec
    def recurse(intersections: Intersections): (Real, Real) = intersections match {
      case IndexedSeq()         => (0.0, 0.0)
      case intersection +: rest =>
        // if `containers` is still not populated at this point,
        // then there is no containing object
        val n1 =
          if (intersection == hit) {
            containers.lastOption.map(_.material.refractiveIndex).getOrElse(Refraction.index.Default)
          } else 0.0

        if (containers.contains(intersection.obj)) {
          // the intersection must be exiting the object, we should
          // remove it from the list
          containers -= intersection.obj
        } else containers.addOne(intersection.obj)

        if (intersection == hit) {
          val n2 = containers.lastOption.map(_.material.refractiveIndex).getOrElse(Refraction.index.Default)
          (n1, n2)
        } else recurse(rest)
    }

    recurse(intersections)
  }
}
