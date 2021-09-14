package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.{Point, Vec}

trait Plane extends Shape {
  override type S = this.type

  /**
   * Intersects with a transformed ray. If the ray is not
   * transformed yet, which usually is the case, you might
   * need to call [[intersect]] instead.
   */
  override def localIntersect(transformedRay: Ray) = ???

  override def localNormalAt(objectPoint: Point) =
    Vec(0, 1, 0)

  override def withMaterial(newMaterial: Material) = ???

  override def withTransformation(newTransformation: Matrix) = ???
}

object Plane {
  def apply(): Plane = new Plane {}
}
