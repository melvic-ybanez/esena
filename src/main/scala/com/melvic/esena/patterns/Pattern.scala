package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.Point

trait Pattern {
  type P

  def transformation: Matrix = Matrix.Identity4x4

  def transform(transformation: Matrix): P

  def applyAt(point: Point): Color

  def applyAt(obj: Shape, worldPoint: Point): Color = {
    val objectPoint = obj.transformation.inverse * worldPoint
    val patternPoint = transformation.inverse * objectPoint
    applyAt(patternPoint)
  }
}

object Pattern {
  trait Aux[A] extends Pattern {
    type P = A
  }

  final case class StripePattern(
      first: Color,
      second: Color,
      override val transformation: Matrix = Matrix.Identity4x4
  ) extends Pattern.Aux[StripePattern] {
    override def applyAt(point: Point): Color =
      if (math.floor(point.x) % 2 == 0) first else second

    override def transform(transformation: Matrix): StripePattern =
      copy(transformation = transformation * this.transformation)
  }
}
