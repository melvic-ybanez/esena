package com.melvic.esena.canvas

import com.melvic.esena.{MathUtils, Real}
import com.melvic.esena.tuples.Tuple

final case class Color(red: Real, green: Real, blue: Real) extends Tuple {
  override val x = red
  override val y = green
  override val z = blue
  override val w = 0

  def hadamardProduct(that: Color): Color =
    Color(
      red * that.red,
      green * that.green,
      blue * that.blue
    )

  /**
   * An alias for the [[hadamardProduct]]
   */
  def *(that: Color): Color = hadamardProduct(that)

  def ppmString(maxValue: Int): String = {
    val r = MathUtils.scaleTo(maxValue, red)
    val g = MathUtils.scaleTo(maxValue, green)
    val b = MathUtils.scaleTo(maxValue, blue)
    s"$r $g $b"
  }
}

object Color {
  val Red = Color(1, 0, 0)
  val Green = Color(0, 1, 0)
  val Blue = Color(0, 0, 1)
  val Black = Color(0, 0, 0)
  val White = Color(1, 1, 1)
}
