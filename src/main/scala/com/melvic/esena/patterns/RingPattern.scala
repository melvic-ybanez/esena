package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.tuples.Point

case class RingPattern(first: Color, second: Color) extends Pattern.Aux[RingPattern] {
  override def at(point: Point) =
    if (math.floor(math.sqrt(math.pow(point.x, 2) + math.pow(point.z, 2))) % 2 == 0)
      first
    else second

  override def withTransformation(transformation: Matrix) =
    RingPattern(first, second, transformation)
}

object RingPattern {
  def apply(first: Color, second: Color, initTransformation: Matrix): RingPattern =
    new RingPattern(first, second) {
      override def transformation = initTransformation
    }
}
