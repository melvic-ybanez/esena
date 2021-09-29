package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.shapes.Group.GroupImpl
import com.melvic.esena.tuples.{Point, Vec}

class Group(children: Vector[Shape]) extends Shape.Aux[Group] {
  def +(child: Shape): Group =
    new Group(child.withParent(this) +: children)

  def addOne(child: Shape): Group =
    this + child

  def addMany(child: Shape*): Group =
    child.foldLeft(this)((g, c) => g + c)

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(transformedRay: Ray) =
    children
      .foldLeft(Intersections.None) {
        case (xs, child) =>
          val intersections = child.intersect(transformedRay)
          intersections ++ xs
      }
      .sortBy(_.t)

  override def localNormalAt(objectPoint: Point) = Vec.Zero

  override def fromData(data: Shape.Data) =
    GroupImpl(children, data.material, data.transformation, data.parent)

  def isEmpty: Boolean = children.isEmpty

  def contains(shape: Shape): Boolean =
    children.contains(shape)
}

object Group {
  case class GroupImpl(
      children: Vector[Shape],
      override val material: Material,
      override val transformation: Matrix,
      override val parent: Option[Group]
  ) extends Group(children)

  def default: Group = new Group(Vector.empty)
}
