package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Point

trait TestPattern extends Pattern.Aux[TestPattern] {
  override def transform(transformation: Matrix) =
    TestPattern(transformation)

  override def at(point: Point) =
    Color(point.x, point.y, point.z)

  def withTransformation(transformation: Matrix): TestPattern =
    TestPattern(transformation)
}

object TestPattern extends TestPattern {
  def apply(): TestPattern = new TestPattern {}

  def apply(initTransformation: Matrix): TestPattern =
    new TestPattern {
      override def transformation = initTransformation
    }
}
