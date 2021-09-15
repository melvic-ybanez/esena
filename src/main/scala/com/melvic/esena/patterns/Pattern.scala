package com.melvic.esena.patterns

import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.{CanTransform, Matrix}
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.Point

trait Pattern extends CanTransform {
  def at(point: Point): Color

  def at(obj: Shape, worldPoint: Point): Color = {
    val objectPoint  = obj.transformation.inverse * worldPoint
    val patternPoint = transformation.inverse * objectPoint
    at(patternPoint)
  }
}

object Pattern {
  trait Aux[A] extends Pattern {
    type T = A
  }
}
