package com.melvic.esena.shapes
import com.melvic.esena.MathUtils
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.tuples.Point

trait Cube extends Shape.Aux[Cube] {

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(ray: Ray): Intersections = {
    val (xtMin, xtMax) = checkAxis(ray.origin.x, ray.direction.x)
    val (ytMin, ytMax) = checkAxis(ray.origin.y, ray.direction.y)
    val (ztMin, ztMax) = checkAxis(ray.origin.z, ray.direction.z)

    // largest minimum value
    val tMin = math.max(xtMin, math.max(ytMin, ztMin))
    // smallest maximum value
    val tMax = math.min(xtMax, math.min(ytMax, ztMax))

    if (tMin > tMax) Vector.empty   // no intersections
    else Intersections(tMin -> this, tMax -> this)
  }

  private def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val tMinNumerator = -1 - origin
    val tMaxNumerator = 1 - origin

    val (tMin, tMax) =
      if (math.abs(direction) >= MathUtils.Epsilon)
        (tMinNumerator / direction, tMaxNumerator / direction)
      else (tMinNumerator * Double.PositiveInfinity, tMaxNumerator * Double.PositiveInfinity)
    if (tMin > tMax) (tMax, tMin) else (tMin, tMax)
  }

  override def localNormalAt(objectPoint: Point) = ???

  override def withMaterial(newMaterial: Material) =
    Cube(newMaterial, transformation)

  override def withTransformation(transformation: Matrix) =
    Cube(material, transformation)
}

object Cube extends Cube {
  def apply(): Cube = new Cube {}

  def apply(initMaterial: Material, initTransformation: Matrix): Cube =
    new Cube {
      override def transformation = initTransformation

      override def material = initMaterial
    }
}
