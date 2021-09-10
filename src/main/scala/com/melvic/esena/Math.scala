package com.melvic.esena

object Math {
  def compareDoubles(a: Double, b: Double): Boolean = {
    val epsilon = 0.000001
    math.abs(a - b) < epsilon
  }

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
  def roundTo5(value: Double): Double =
    math.round(value * 1e5) / 1e5
}
