package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Point

case class GradientPattern(from: Color, to: Color) extends Pattern.Aux[GradientPattern] {

  /**
    * Interpolates the values of two colors using
    * a basic linear interpolation formula
    */
  override def at(point: Point) = {
    val distance = to - from
    val fraction = point.x - math.floor(point.x)
    from + distance * fraction
  }

  override def withTransformation(transformation: Matrix) =
    GradientPattern(from, to, transformation)
}

object GradientPattern {
  def apply(from: Color, to: Color, initTransformation: Matrix): GradientPattern =
    new GradientPattern(from, to) {
      override def transformation = initTransformation
    }
}
