package com.melvic.esena.shapes
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.rays.Ray

trait TestShape extends Shape {
  override type S = TestShape

  override def intersectWithTransformedRay(ray: Ray) = ???

  override def withMaterial(newMaterial: Material) =
    TestShape(newMaterial, transformation)

  override def withTransformation(newTransformation: Matrix) =
    TestShape(material, newTransformation)
}

object TestShape {
  def apply() = new TestShape {}

  def apply(initMaterial: Material, initTransformation: Matrix): TestShape =
    new TestShape {
      override def material = initMaterial

      override def transformation = initTransformation
    }
}
