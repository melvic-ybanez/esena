package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.{CanTransform, Matrix}
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.Point

trait Pattern extends CanTransform {
  def at(point: Point): Color

  def at(obj: Shape, worldPoint: Point): Color = {
    val objectPoint = obj.transformation.inverse * worldPoint
    val patternPoint = transformation.inverse * objectPoint
    at(patternPoint)
  }
}

object Pattern {
  trait Aux[A] extends Pattern {
    type T = A
  }

  final case class StripePattern(
      first: Color,
      second: Color,
      override val transformation: Matrix = Matrix.Identity4x4
  ) extends Pattern.Aux[StripePattern] {
    override def at(point: Point): Color =
      if (math.floor(point.x) % 2 == 0) first else second

    override def withTransformation(transformation: Matrix) =
      copy(transformation = transformation)
  }

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
}
