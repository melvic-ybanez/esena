package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.TestShape.TestShapeImpl
import com.melvic.esena.tuples.{Point, Vec}

trait TestShape extends Shape.Aux[TestShape] {
  // for testing purposes only, we are caching the transformed ray
  var transformedRay: Ray = Ray(Point.Origin, Vec.Zero)

  override def localIntersect(ray: Ray) = {
    transformedRay = ray
    Vector()
  }

  override def localNormalAt(objectPoint: Point) =
    objectPoint.toVec

  override def fromData(data: Shape.Data) =
    TestShapeImpl(data.material, data.transformation, data.parent)
}

object TestShape extends TestShape {
  case class TestShapeImpl(
      override val material: Material,
      override val transformation: Matrix,
      override val parent: Option[Group]
  ) extends TestShape
}
