package com.melvic.esena.matrix

trait Transformations4D {

  /**
    * Increases each of the elements of a point by multiplying
    * it with the following matrix:
    * [ 1 0 0 x ]
    * [ 0 1 0 y ]
    * [ 0 0 1 z ]
    * [ 0 0 0 1 ]
    * Note: Applying this to a vector shouldn't alter the vector
    * because the vector's fourth component is 0 (it cancels the fourth
    * column)
    */
  def translation(x: Double, y: Double, z: Double): Matrix =
    Matrix.identity4By4(0, 3, x)(1, 3, y)(2, 3, z)

  /**
    * Multiplies each component of a tuple by its corresponding scalar.
    * Scaling matrix:
    * [ x, 0, 0, 0 ]
    * [ 0, y, 0, 0 ]
    * [ 0, 0, z, 0 ]
    * [ 0, 0, 0, 1 ]
    * This works for both point and vector.
    */
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

  /**
    * Clockwise rotations along the y-axis.
    * Note: This rotation is based on the left-hand rule.
    */
  def rotationY(radian: Double): Matrix = {
    val cos = math.cos(radian)
    val sin = math.sin(radian)

    Matrix.of4By4(
      (cos, 0, sin, 0),
      (0, 1, 0, 0),
      (-sin, 0, cos, 0),
      (0, 0, 0, 1)
    )
  }

  /**
    * Clockwise rotations along the z-axis.
    * Note: This rotation is based on the left-hand rule.
    */
  def rotationZ(radian: Double): Matrix = {
    val cos = math.cos(radian)
    val sin = math.sin(radian)

    Matrix.of4By4(
      (cos, -sin, 0, 0),
      (sin, cos, 0, 0),
      (0, 0, 1, 0),
      (0, 0, 0, 1)
    )
  }

  def shearing(xy: Double, xz: Double, yx: Double, yz: Double, zx: Double, zy: Double): Matrix =
    Matrix.of4By4(
      (1, xy, xz, 0),
      (yx, 1, yz, 0),
      (zx, zy, 1, 0),
      (0, 0, 0, 1)
    )
}
