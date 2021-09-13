package com.melvic.esena

object MathUtils {
  val Epsilon = 0.000001

  def compareDoubles(a: Double, b: Double): Boolean =
    math.abs(a - b) < Epsilon

  def scaleTo(maxValue: Int, value: Double): Int = {
    val result = ((maxValue + 1) * value).toInt
    if (result > maxValue) maxValue
    else if (result < 0) 0
    else result
  }

  def indexOf(row: Int, col: Int, width: Int): Int =
    row * width + col

  /**
   * This is useful when testing with rounded values
   */
  def roundTo(exp: Int)(value: Double): Double = {
    val e = math.pow(10, exp)
    math.round(value * e) / e
  }

  def roundTo5: Double => Double = roundTo(5)
}
