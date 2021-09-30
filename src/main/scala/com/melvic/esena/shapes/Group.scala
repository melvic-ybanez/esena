package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.shapes.Group.GroupImpl
import com.melvic.esena.tuples.{Point, Vec}

class Group(val children: Vector[Shape]) extends Shape.Aux[Group] {
  def +(child: Shape): Group =
    fromData(data).copy(children = child.withParent(this) +: children)

  def addOne(child: Shape): Group =
    this + child

  def addMany(child: Shape*): Group =
    child.foldLeft(this)((g, c) => g + c)

  override def intersect(ray: Ray) =
    localIntersect(ray)   // do not transform the ray beforehand

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  override def localIntersect(ray: Ray) =
    children
      .foldLeft(Intersections.None) {
        case (xs, child) =>
          val intersections = child.transform(transformation).intersect(ray)
          intersections ++ xs
      }
      .sortBy(_.t)

  override def localNormalAt(objectPoint: Point) = Vec.Zero

  override def fromData(data: Shape.Data) =
    GroupImpl(
      children = children,
      material = data.material,
      transformation = data.transformation,
      parent = data.parent
    )

  def isEmpty: Boolean = children.isEmpty

  def contains(shape: Shape): Boolean =
    children.contains(shape)
}

object Group extends Group(Vector.empty) {
  case class GroupImpl(
      override val children: Vector[Shape],
      override val material: Material,
      override val transformation: Matrix,
      override val parent: Option[Group]
  ) extends Group(children)

  def default: Group = Group
}
