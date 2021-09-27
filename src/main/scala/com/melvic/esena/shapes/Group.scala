package com.melvic.esena.shapes

import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.Point

final case class Group(shapes: Vector[Shape]) extends Shape.Aux[Group] {
  def isEmpty: Boolean = shapes.isEmpty

  /**
   * Intersects with a transformed ray. If the ray is not
   * transformed yet, which usually is the case, you might
   * need to call [[intersect]] instead.
   */
  override def localIntersect(transformedRay: Ray) = ???

  override def localNormalAt(objectPoint: Point) = ???

  override def fromData(data: Shape.Data) = ???
}

object Group {
  def default: Group = Group(Vector.empty)
}