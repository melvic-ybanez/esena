package com.melvic.esena.shapes

import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.{CanTransform, Matrix}
import com.melvic.esena.rays.Intersections.Intersections
import com.melvic.esena.rays.{CanIntersect, Ray}
import com.melvic.esena.shapes.Shape.Data
import com.melvic.esena.tuples.{Point, Vec}

trait Shape extends CanIntersect with CanTransform {
  type T <: Shape

  def material: Material = Material()

  def parent: Option[Group] = None

  /**
    * Intersects with a transformed ray. If the ray is not
    * transformed yet, which usually is the case, you might
    * need to call [[intersect]] instead.
    */
  def localIntersect(transformedRay: Ray): Intersections

  def localNormalAt(objectPoint: Point): Vec

  def updateMaterial(f: Material => Material): T =
    withMaterial(f(material))

  def intersect(ray: Ray): Intersections = {
    // transform the ray into object space
    val transformedRay = ray.transform(transformation.inverse)

    localIntersect(transformedRay)
  }

  def normalAt(worldPoint: Point): Vec = {
    val objectPoint    = transformation.inverse * worldPoint
    val worldNormal    = transformation.inverse.transpose * localNormalAt(objectPoint)
    val worldNormalVec = worldNormal.toVec // sets the w to 0
    worldNormalVec.normalize
  }

  val data: Data = Data(
    material = material,
    transformation = transformation,
    parent = parent
  )

  def fromData(data: Data): T

  def update(f: Data => Data): T =
    fromData(f(data))

  def withMaterial(material: Material): T =
    fromData(data.copy(material = material))

  def withTransformation(transformation: Matrix): T =
    fromData(data.copy(transformation = transformation))

  def withParent(parent: Group): T =
    fromData(data.copy(parent = Some(parent)))

  override def equals(o: Any) = o match {
    case shape: Shape =>
      shape.transformation == transformation && shape.material == material
    case _            => false
  }

  override def toString =
    s"[material = $material, transformation = $transformation, parent = $parent]"
}

object Shape {
  trait Aux[A <: Shape] extends Shape {
    override type T = A
  }

  case class Data(material: Material, transformation: Matrix, parent: Option[Group])
}
