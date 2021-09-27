package com.melvic.esena.shapes
import com.melvic.esena.MathUtils.pow2
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cylinder.CylinderImpl
import com.melvic.esena.tuples.Point

trait Cylinder extends LeafShape[Cylinder] with CylinderLike {
  def localIntersect(ray: Ray): Intersections = {
    val a = pow2(ray.direction.x) + pow2(ray.direction.z)
    val b = 2 * ray.origin.x * ray.direction.x + 2 * ray.origin.z * ray.direction.z
    val c = pow2(ray.origin.x) + pow2(ray.origin.z) - 1
    localIntersectWith(ray, a, b, c)
  }

  override def fromData(data: Data) =
    CylinderImpl(data.min, data.max, data.closed, data.material, data.transformation)

  override def radius(y: Double) = 1

  override def computeNormalY(point: Point) = 0
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
