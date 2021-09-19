package com.melvic.esena.shapes
import com.melvic.esena.MathUtils
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersection, Ray}
import com.melvic.esena.tuples.{Point, Vec}

/**
  * A plane in xz, extending infinitely to both x and z directions.
  */
trait Plane extends Shape.Aux[Plane] {

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(transformedRay: Ray): Intersections =
    if (math.abs(transformedRay.direction.y) < MathUtils.Epsilon) {
      // y is practically 0 in this case. Either the ray is parallel
      // to the plane (in which case it misses the plane entirely)
      // or it's coplanar with it. If it's the latter, then it technically
      // doesn't miss the plane (in fact there would be infinite number of
      // intersections), but it would be invisible because of how thin the plane is
      Vector.empty
    } else Intersection(-transformedRay.origin.y / transformedRay.direction.y, this) +: Vector()

  override def localNormalAt(objectPoint: Point) =
    Vec(0, 1, 0)

  override def withMaterial(newMaterial: Material) =
    Plane(newMaterial, transformation)

  override def withTransformation(newTransformation: Matrix) =
    Plane(material, newTransformation)
}

object Plane extends Plane {
  def apply(): Plane = new Plane {}

  def apply(initMaterial: Material, initTransformation: Matrix): Plane =
    new Plane {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
