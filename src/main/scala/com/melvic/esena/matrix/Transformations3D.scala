package com.melvic.esena.matrix

trait Transformations3D {
  def translate(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 3, x)(1, 3, y)(2, 3, z)
}
