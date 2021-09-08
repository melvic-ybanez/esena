package com.melvic.esena.matrix

trait Transformations4D {
  def translation(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 3, x)(1, 3, y)(2, 3, z)

  def scaling(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 0, x)(1, 1, y)(2, 2, z)

  /**
    * Clockwise rotations along the x-axis.
    * Note: This rotation is based on the left-hand rule.
    */
  def rotationX(radian: Double): Matrix = {
    val cos = math.cos(radian)
    val sin = math.sin(radian)

    Matrix.of4By4(
      (1, 0, 0, 0),
      (0, cos, -sin, 0),
      (0, sin, cos, 0),
      (0, 0, 0, 1)
    )
  }
}
