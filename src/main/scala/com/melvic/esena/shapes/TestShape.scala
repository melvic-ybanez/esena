package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.{Point, Vec}

trait TestShape extends Shape.Aux[TestShape] {
  // for testing purposes only, we are caching the transformed ray
  var transformedRay: Ray = Ray(Point.Origin, Vec.Zero)

  override def localIntersect(ray: Ray) = {
    transformedRay = ray
    Vector()
  }

  override def withMaterial(newMaterial: Material) =
    TestShape(newMaterial, transformation)

  override def withTransformation(newTransformation: Matrix) =
    TestShape(material, newTransformation)

  override def localNormalAt(objectPoint: Point) =
    objectPoint.toVec
}

object TestShape {
  def apply() = new TestShape {}

  def apply(initMaterial: Material, initTransformation: Matrix): TestShape =
    new TestShape {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
