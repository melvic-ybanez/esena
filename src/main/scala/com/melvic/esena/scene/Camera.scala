package com.melvic.esena.scene

import com.melvic.esena.matrix.Matrix
import com.melvic.esena.matrix.Matrix.Identity4x4

final case class Camera(
    hSize: Double,
    vSize: Double,
    fieldOfView: Double,
    transformation: Matrix = Identity4x4
) {

  /**
    * Cutting the field of view in half creates a right triangle,
    * so half-view = tan(field-of-view / 2) / distance-from-the-eye,
    * and the canvas is only 1 unit away from the eye
    */
  def halfView: Double = math.tan(fieldOfView / 2)

  val aspectRatio: Double = hSize / vSize

  def halfWidth: Double =
    if (aspectRatio >= 1) halfView else halfView * aspectRatio

  def halfHeight: Double =
    if (aspectRatio >= 1) halfView / aspectRatio else halfView

  /**
    * Note: A pixel is a square so the vertical size and horizontal size
    * are the same. The formula could also be `halfHeight * 2 / vSize`
    */
  def pixelSize: Double =
    halfWidth * 2 / hSize
}
