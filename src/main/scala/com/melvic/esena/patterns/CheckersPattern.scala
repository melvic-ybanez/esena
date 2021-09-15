package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Point

case class CheckersPattern(first: Color, second: Color) extends Pattern.Aux[CheckersPattern] {

  /**
    * Just like the stripe pattern, except it now considers all three dimensions
    * instead of just a single one.
    */
  override def at(point: Point) = {
    val dimensionsSum = math.floor(point.x) + math.floor(point.y) + math.floor(point.z)
    if (dimensionsSum % 2 == 0) first else second
  }

  override def withTransformation(transformation: Matrix) =
    CheckersPattern(first, second, transformation)
}

object CheckersPattern {
  def apply(first: Color, second: Color, initTransformation: Matrix): CheckersPattern =
    new CheckersPattern(first, second) {
      override def transformation = initTransformation
    }
}
