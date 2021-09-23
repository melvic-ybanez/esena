package com.melvic.esena.shapes
import com.melvic.esena.MathUtils.{compareDoubles, squared}
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.tuples.{Point, Vec}

case class Cylinder(min: Double = Double.NegativeInfinity, max: Double = Double.PositiveInfinity)
    extends Shape.Aux[Cylinder] {

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(ray: Ray): Intersections = {
    val a = squared(ray.direction.x) + squared(ray.direction.z)

    if (compareDoubles(a, 0)) Vector.empty // ray is parallel to the y-axis
    else {
      val b = 2 * ray.origin.x * ray.direction.x +
        2 * ray.origin.z * ray.direction.z
      val c            = squared(ray.origin.x) + squared(ray.origin.z) - 1
      val discriminant = squared(b) - 4 * a * c

      if (discriminant < 0) Vector.empty // ray does not intersect this cylinder
      else {
        val t0 = (-b - math.sqrt(discriminant)) / (2 * a)
        val t1 = (-b + math.sqrt(discriminant)) / (2 * a)
        Intersections(t0 -> this, t1 -> this)
      }
    }
  }

  override def localNormalAt(objectPoint: Point): Vec = {
    // simply removes the y-component
    Vec(objectPoint.x, 0, objectPoint.z)
  }

  override def withMaterial(newMaterial: Material) =
    Cylinder(min, max, newMaterial, transformation)

  override def withTransformation(transformation: Matrix) =
    Cylinder(min, max, material, transformation)
}

object Cylinder {
  def apply(min: Double, max: Double, initMaterial: Material, initTransformation: Matrix): Cylinder =
    new Cylinder(min, max) {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
