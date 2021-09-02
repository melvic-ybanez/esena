package com.melvic.esena

final case class Point(x: Double, y: Double, z: Double) extends Tuple {
  val w: Double = 1
}
