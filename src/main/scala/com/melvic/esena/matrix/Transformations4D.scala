package com.melvic.esena.matrix

import com.melvic.esena.tuples.{Point, Vec}

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
    Matrix.Identity4x4(0, 3, x)(1, 3, y)(2, 3, z)

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
    Matrix.Identity4x4(0, 0, x)(1, 1, y)(2, 2, z)

  /**
    * Clockwise rotations along the x-axis.
    * Note: This rotation is based on the left-hand rule.
    */
  def rotationX(radian: Double): Matrix = {
    val cos = math.cos(radian)
    val sin = math.sin(radian)

    Matrix.of4x4(
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

    Matrix.of4x4(
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

    Matrix.of4x4(
      (cos, -sin, 0, 0),
      (sin, cos, 0, 0),
      (0, 0, 1, 0),
      (0, 0, 0, 1)
    )
  }

  def shearing(xy: Double, xz: Double, yx: Double, yz: Double, zx: Double, zy: Double): Matrix =
    Matrix.of4x4(
      (1, xy, xz, 0),
      (yx, 1, yz, 0),
      (zx, zy, 1, 0),
      (0, 0, 0, 1)
    )

  /**
    * Orients the world relative to the eye
    * @param from point of the eye in the scene
    * @param to point you want to look at
    * @param up indicates which direction is up
    */
  def view(from: Point, to: Point, up: Vec): Matrix = {
    val forward = (to - from).normalize
    val left = forward.cross(up.normalize)
    val trueUp = left.cross(forward)

    val orientation = Matrix.of4x4(
      (left.x, left.y, left.z, 0),
      (trueUp.x, trueUp.y, trueUp.z, 0),
      (-forward.x, -forward.y, -forward.z, 0),
      (0, 0, 0, 1)
    )

    // move the scene into place
    val trans = translation(-from.x, -from.y, -from.z)

    orientation * trans
  }
}

object Transformations4D extends Transformations4D
