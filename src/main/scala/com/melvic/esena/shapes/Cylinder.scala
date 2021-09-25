package com.melvic.esena.shapes
import com.melvic.esena.MathUtils
import com.melvic.esena.MathUtils.{compareDoubles, pow2}
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersection, Intersections, Ray}
import com.melvic.esena.tuples.{Point, Vec}

case class Cylinder(
    min: Double = Double.NegativeInfinity,
    max: Double = Double.PositiveInfinity,
    closed: Boolean = false
) extends Shape.Aux[Cylinder] {

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(ray: Ray): Intersections = {
    val a = pow2(ray.direction.x) + pow2(ray.direction.z)

    if (compareDoubles(a, 0)) // ray is parallel to the y-axis
      intersectCaps(ray, Intersections.None)
    else {
      val b = 2 * ray.origin.x * ray.direction.x +
        2 * ray.origin.z * ray.direction.z
      val c            = pow2(ray.origin.x) + pow2(ray.origin.z) - 1
      val discriminant = pow2(b) - 4 * a * c

      if (discriminant < 0) Intersections.None // ray does not intersect this cylinder
      else {
        val t0Temp = (-b - math.sqrt(discriminant)) / (2 * a)
        val t1Temp = (-b + math.sqrt(discriminant)) / (2 * a)

        val (t0, t1) = if (t0Temp > t1Temp) (t1Temp, t0Temp) else (t0Temp, t1Temp)

        def yBetweenAtT(t: Double): Intersections = {
          val y = ray.origin.y + t * ray.direction.y
          Intersections.maybeOne(min < y && y < max, t -> this)
        }

        intersectCaps(ray, yBetweenAtT(t0) ++ yBetweenAtT(t1))
      }
    }
  }

  /**
    * Checks if the intersection is within the radius. If it is, include the
    * intersection.
    */
  private def checkCap(ray: Ray, limit: Double, xs: Intersections): Intersections = {
    val t = (limit - ray.origin.y) / ray.direction.y
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z

    if ((pow2(x) + pow2(z)) <= Radius) Intersection(t, this) +: xs
    else xs
  }

  private def intersectCaps(ray: Ray, xs: Intersections): Intersections =
    if (!closed || MathUtils.compareDoubles(ray.direction.y, 0)) xs
    else checkCap(ray, max, checkCap(ray, min, xs))

  override def localNormalAt(point: Point): Vec = {
    val Point(x, y, z) = point

    // square of the distance from the y-axis
    val dist = pow2(x) + pow2(z)

    if (dist < 1 && y >= max - MathUtils.Epsilon)
      Vec(0, 1, 0)
    else if (dist < 1 && y <= min + MathUtils.Epsilon)
      Vec(0, -1, 0)
    // simply removes the y-component
    else Vec(point.x, 0, point.z)
  }

  override def withMaterial(newMaterial: Material) =
    Cylinder(min, max, closed, newMaterial, transformation)

  override def withTransformation(transformation: Matrix) =
    Cylinder(min, max, closed, material, transformation)
}

object Cylinder {
  def apply(
      min: Double,
      max: Double,
      closed: Boolean,
      initMaterial: Material,
      initTransformation: Matrix
  ): Cylinder =
    new Cylinder(min, max, closed) {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
