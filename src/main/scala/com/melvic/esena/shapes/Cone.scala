package com.melvic.esena.shapes
import com.melvic.esena.{MathUtils, Real}
import com.melvic.esena.MathUtils.pow2
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.shapes.Cone.ConeImpl
import com.melvic.esena.tuples.Point

trait Cone extends Shape.Aux[Cone] with CylinderLike {
  override def localIntersect(transformedRay: Ray) = {
    val Ray(o, d) = transformedRay
    val a = pow2(d.x) - pow2(d.y) + pow2(d.z)
    val b = 2 * o.x * d.x - 2 * o.y * d.y + 2 * o.z * d.z
    val c = pow2(o.x) - pow2(o.y) + pow2(o.z)

    val aIs0 = math.abs(a) < MathUtils.Epsilon
    if (aIs0 && (math.abs(b) > MathUtils.Epsilon)) {
      val t = -c / (2 * b)
      Intersections(t -> this)
    } else if (!aIs0) localIntersectWith(transformedRay, a, b, c)
    else Intersections.None
  }

  override def radius(y: Real) = math.abs(y)

  override def fromData(data: Data) =
    ConeImpl(data.min, data.max, data.closed, data.material, data.transformation)

  override def computeNormalY(point: Point) = {
    val y = math.sqrt(pow2(point.x) + pow2(point.z))
    if (point.y > 0) -y else y
  }
}

object Cone extends Cone {
  case class ConeImpl(
      override val min: Real,
      override val max: Real,
      override val closed: Boolean,
      override val material: Material,
      override val transformation: Matrix,
  ) extends Cone
}
