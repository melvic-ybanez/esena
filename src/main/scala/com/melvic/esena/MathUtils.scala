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
    * Rounds a value based on a given number of digits.
    * Note: This is useful when testing with rounded values
    */
  def roundTo(digitCount: Int)(value: Double): Double = {
    val e = math.pow(10, digitCount)
    math.round(value * e) / e
  }

  def roundTo5: Double => Double = roundTo(5)

  def max(x: Double, xs: Double*): Double =
    compare(x, xs: _*)(math.max)

  def min(x: Double, xs: Double*): Double =
    compare(x, xs: _*)(math.min)

  def pow2(xs: Double): Double = math.pow(xs, 2)

  private def compare(x: Double, xs: Double*)(f: (Double, Double) => Double): Double =
    xs.foldLeft(x) { (result, x) =>
      f(result, x)
    }
}
