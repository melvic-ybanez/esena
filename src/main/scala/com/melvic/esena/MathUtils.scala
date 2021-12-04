package com.melvic.esena

object MathUtils {
  type Real = Double
  val Real = Double

  val Epsilon = 0.000001

  def compareReals(a: Real, b: Real): Boolean = {
    val diff = math.abs(a - b)
    diff.isNaN || diff < Epsilon
  }

  def scaleTo(maxValue: Int, value: Real): Int = {
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
  def roundTo(digitCount: Int)(value: Real): Real = {
    val e = math.pow(10, digitCount)
    math.round(value * e) / e
  }

  def roundTo5: Real => Real = roundTo(5)

  def max(x: Real, xs: Real*): Real =
    compare(x, xs: _*)(math.max)

  def min(x: Real, xs: Real*): Real =
    compare(x, xs: _*)(math.min)

  def pow2(xs: Real): Real = math.pow(xs, 2)

  private def compare(x: Real, xs: Real*)(f: (Real, Real) => Real): Real =
    xs.foldLeft(x) { (result, x) =>
      f(result, x)
    }
}
