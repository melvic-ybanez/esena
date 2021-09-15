package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.rays.Intersection.Intersections
import com.melvic.esena.rays.{CanIntersect, Ray}
import com.melvic.esena.tuples.{Point, Vec}

trait Shape extends CanIntersect {
  type S

  def transformation: Matrix = Identity4x4

  def material: Material = Material()

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  def localIntersect(transformedRay: Ray): Intersections

  def localNormalAt(objectPoint: Point): Vec

  def withMaterial(newMaterial: Material): S

  def withTransformation(newTransformation: Matrix): S

  /**
    * Adds a transformation on top of the already existing one
    */
  def transform(transformation: Matrix): S =
    withTransformation(transformation * this.transformation)

  def intersect(ray: Ray): Intersections = {
    // transform the ray into object space
    val transformedRay = ray.transform(transformation.inverse)

    localIntersect(transformedRay)
  }

  def normalAt(worldPoint: Point): Vec = {
    val objectPoint    = transformation.inverse * worldPoint
    val worldNormal    = transformation.inverse.transpose * localNormalAt(objectPoint)
    val worldNormalVec = worldNormal.toVec // sets the w to 0
    worldNormalVec.normalize
  }

  override def equals(o: Any) = o match {
    case shape: Shape => shape.transformation == transformation && shape.material == material
    case _            => false
  }
}

object Shape {
  trait Aux[A] extends Shape {
    type S = A
  }
}
