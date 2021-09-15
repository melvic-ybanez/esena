package com.melvic.esena.matrix

trait CanTransform {
  type T

  def transformation: Matrix = Matrix.Identity4x4

  def withTransformation(transformation: Matrix): T

  /**
   * Adds a transformation on top of the already existing one
   */
  def transform(transformation: Matrix): T =
    withTransformation(transformation * this.transformation)
}
