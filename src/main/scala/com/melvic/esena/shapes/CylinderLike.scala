package com.melvic.esena.shapes

import com.melvic.esena.{MathUtils, Real}
import com.melvic.esena.MathUtils.{compareReals, pow2}
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersection, Intersections, Ray}
import com.melvic.esena.tuples.{Point, Vec}

trait CylinderLike extends { self: Shape =>
  def min: Real = Real.NegativeInfinity

  def max: Real = Real.PositiveInfinity

  def closed: Boolean = false

  def radius(y: Real): Real

  def localIntersectWith(ray: Ray, a: Real, b: Real, c: Real): Intersections =
    if (compareReals(a, 0)) // ray is parallel to the y-axis
      intersectCaps(ray, Intersections.None)
    else {
      val discriminant = pow2(b) - 4 * a * c

      if (discriminant < 0) Intersections.None // ray does not intersect this cylinder
      else {
        val t0Temp = (-b - math.sqrt(discriminant)) / (2 * a)
        val t1Temp = (-b + math.sqrt(discriminant)) / (2 * a)

        val (t0, t1) = if (t0Temp > t1Temp) (t1Temp, t0Temp) else (t0Temp, t1Temp)

        def yBetweenAtT(t: Real): Intersections = {
          val y = ray.origin.y + t * ray.direction.y
          Intersections.maybeOne(min < y && y < max, t -> this)
        }

        intersectCaps(ray, yBetweenAtT(t0) ++ yBetweenAtT(t1))
      }
    }

  /**
    * Checks if the intersection is within the radius. If it is, include the
    * intersection.
    */
  private def checkCap(ray: Ray, limit: Real, xs: Intersections): Intersections = {
    val t = (limit - ray.origin.y) / ray.direction.y
    val x = ray.origin.x + t * ray.direction.x
    val z = ray.origin.z + t * ray.direction.z

    if ((pow2(x) + pow2(z)) <= radius(limit)) Intersection(t, this) +: xs
    else xs
  }

  private def intersectCaps(ray: Ray, xs: Intersections): Intersections =
    if (!closed || MathUtils.compareReals(ray.direction.y, 0)) xs
    else checkCap(ray, max, checkCap(ray, min, xs))

  def localNormalAt(point: Point): Vec = {
    val Point(x, y, z) = point

    // square of the distance from the y-axis
    val dist = pow2(x) + pow2(z)

    if (dist < 1 && y >= max - MathUtils.Epsilon)
      Vec(0, 1, 0)
    else if (dist < 1 && y <= min + MathUtils.Epsilon)
      Vec(0, -1, 0)
    // simply removes the y-component
    else Vec(point.x, computeNormalY(point), point.z)
  }

  def computeNormalY(point: Point): Real

  case class Data(
      min: Real = self.min,
      max: Real = self.max,
      closed: Boolean = self.closed,
      material: Material = self.material,
      transformation: Matrix = self.transformation
  )

  def fromData(data: Data): T

  override def fromData(data: Shape.Data): T =
    fromData(Data(material = data.material, transformation = data.transformation))

  def update(f: Data => Data)(implicit dummyImplicit: DummyImplicit): T =
    fromData(f(Data()))

  def withMin(min: Real): T =
    fromData(Data(min = min))

  def withMax(max: Real): T =
    fromData(Data(max = max))

  def withClosed(closed: Boolean): T =
    fromData(Data(closed = closed))
}
