package com.melvic.esena

final case class Color(red: Double, green: Double, blue: Double) extends Tuple {
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
}

object Color {
  val Red = Color(1, 0, 0)
  val Green = Color(0, 1, 0)
  val Blue = Color(0, 0, 1)
  val Black = Color(0, 0, 0)
}
