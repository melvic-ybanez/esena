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

  def translate(x: Double, y: Double, z: Double): T =
    transform(translation(x, y, z))

  def scale(x: Double, y: Double, z: Double): T =
    transform(scaling(x, y, z))

  def rotateX(radian: Double): T =
    transform(rotationX(radian))

  def rotateY(radian: Double): T =
    transform(rotationY(radian))

  def rotateZ(radian: Double): T =
    transform(rotationZ(radian))
}
