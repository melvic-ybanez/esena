package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.{CanIntersect, Ray}
import com.melvic.esena.tuples.{Point, Vec}

trait Shape extends CanIntersect {
  def transformation: Matrix

  def material: Material

  def intersectWithTransformedRay(ray: Ray): Intersections

  def intersect(ray: Ray): Intersections = {
    // transform the ray into object space
    val transformedRay = ray.transform(transformation.inverse)

    intersectWithTransformedRay(transformedRay)
  }

  def normalAt(worldPoint: Point): Vec = {
    val objectPoint    = transformation.inverse * worldPoint
    val objectNormal   = objectPoint - Point.Origin
    val worldNormal    = transformation.inverse.transpose * objectNormal
    val worldNormalVec = worldNormal.toVec // sets the w to 0
    worldNormalVec.normalize
  }
}

