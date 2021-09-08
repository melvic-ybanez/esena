package com.melvic.esena.matrix

trait Transformations3D {
  def translation(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 3, x)(1, 3, y)(2, 3, z)

  def scaling(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 0, x)(1, 1, y)(2, 2, z)
}
