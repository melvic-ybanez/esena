package com.melvic.esena.shapes
import com.melvic.esena.MathUtils
import com.melvic.esena.MathUtils.pow2
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cylinder.CylinderImpl
import com.melvic.esena.tuples.{Point, Vec}

trait Cylinder extends Shape.Aux[Cylinder] with CylinderLike {

  /**
    * Computes the value of a needed for the discriminant
    */
  override def computeA(ray: Ray): Double =
    pow2(ray.direction.x) + pow2(ray.direction.z)

  /**
    * Computes the value of b needed for the discriminant
    */
  override def computeB(ray: Ray): Double =
    2 * ray.origin.x * ray.direction.x + 2 * ray.origin.z * ray.direction.z

  /**
    * Computes the value of c needed for the discriminant
    */
  override def computeC(ray: Ray): Double =
    pow2(ray.origin.x) + pow2(ray.origin.z) - 1

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

  override def fromData(data: Data) =
    CylinderImpl(data.min, data.max, data.closed, data.material, data.transformation)
}

object Cylinder extends Cylinder {
  case class CylinderImpl(
      override val min: Double,
      override val max: Double,
      override val closed: Boolean,
      override val material: Material,
      override val transformation: Matrix,
  ) extends Cylinder
}
