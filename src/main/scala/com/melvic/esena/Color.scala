package com.melvic.esena

final case class Color(red: Double, green: Double, blue: Double) extends Tuple {
  override val x = red
  override val y = green
  override val z = blue
  override val w = 0
}
