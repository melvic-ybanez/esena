package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Point

case class StripePattern(first: Color, second: Color) extends Pattern.Aux[StripePattern] {
  override def at(point: Point): Color =
    if (math.floor(point.x) % 2 == 0) first else second

  override def withTransformation(transformation: Matrix) =
    StripePattern(first, second, transformation)
}

object StripePattern {
  def apply(first: Color, second: Color, initTransformation: Matrix): StripePattern =
    new StripePattern(first, second) {
      override def transformation = initTransformation
    }
}
