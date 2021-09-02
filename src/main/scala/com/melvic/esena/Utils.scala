package com.melvic.esena

object Utils {
  def compareDoubles(a: Double, b: Double): Boolean = {
    val epsilon = 0.000001
    math.abs(a - b) < epsilon
  }
}