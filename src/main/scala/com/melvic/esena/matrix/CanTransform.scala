package com.melvic.esena.matrix

import com.melvic.esena.Real

trait CanTransform {
  type T

  def transformation: Matrix = Matrix.Identity4x4

  def withTransformation(transformation: Matrix): T

  /**
    * Adds a transformation on top of the already existing one
    */
  def transform(transformation: Matrix): T =
    withTransformation(transformation * this.transformation)

  def translate(x: Real, y: Real, z: Real): T =
    transform(translation(x, y, z))

  def scale(x: Real, y: Real, z: Real): T =
    transform(scaling(x, y, z))

  def scale(xyz: Real): T = scale(xyz, xyz, xyz)

  def rotateX(radian: Real): T =
    transform(rotationX(radian))

  def rotateY(radian: Real): T =
    transform(rotationY(radian))

  def rotateZ(radian: Real): T =
    transform(rotationZ(radian))
}
