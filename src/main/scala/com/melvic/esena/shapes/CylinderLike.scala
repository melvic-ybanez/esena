package com.melvic.esena.shapes

import com.melvic.esena.MathUtils
import com.melvic.esena.MathUtils.{compareDoubles, pow2}
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersection, Intersections, Ray}

trait CylinderLike extends { self: Shape =>
  def min: Double = Double.NegativeInfinity

  def max: Double = Double.PositiveInfinity

  def closed: Boolean = false

  def localIntersect(ray: Ray): Intersections = {
    val a = computeA(ray)

    if (compareDoubles(a, 0)) // ray is parallel to the y-axis
      intersectCaps(ray, Intersections.None)
    else {
      val b            = computeB(ray)
      val c            = computeC(ray)
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
    * Computes the value of a needed for the discriminant
    */
  def computeA(ray: Ray): Double

  /**
    * Computes the value of b needed for the discriminant
    */
  def computeB(ray: Ray): Double

  /**
    * Computes the value of c needed for the discriminant
    */
  def computeC(ray: Ray): Double

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

  case class Data(
      min: Double = self.min,
      max: Double = self.max,
      closed: Boolean = self.closed,
      material: Material = self.material,
      transformation: Matrix = self.transformation
  )

  def fromData(data: Data): T

  def update(f: Data => Data): T =
    fromData(f(Data()))

  override def withMaterial(material: Material) =
    fromData(Data(material = material))

  override def withTransformation(transformation: Matrix) =
    fromData(Data(transformation = transformation))

  def withMin(min: Double): T =
    fromData(Data(min = min))

  def withMax(max: Double): T =
    fromData(Data(max = max))

  def withClosed(closed: Boolean): T =
    fromData(Data(closed = closed))
}
