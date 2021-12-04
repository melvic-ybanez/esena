package com.melvic.esena.tuples

import com.melvic.esena.Real

final case class Point(x: Real, y: Real, z: Real) extends Tuple {
  val w: Real = 1
}

object Point {
  val Origin = Point(0, 0, 0)
}
