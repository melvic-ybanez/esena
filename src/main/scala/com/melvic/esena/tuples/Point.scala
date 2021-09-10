package com.melvic.esena.tuples

final case class Point(x: Double, y: Double, z: Double) extends Tuple {
  val w: Double = 1
}

object Point {
  val Origin = Point(0, 0, 0)
}
