package com.melvic.esena.scene

import com.melvic.esena.matrix.Matrix
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.rays.Ray
import com.melvic.esena.tuples.Point

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

  def rayForPixel(x: Double, y: Double): Ray = {
    // offset from the edge of the canvas to the pixel's center
    val xOffset = (x + 0.5) * pixelSize
    val yOffset = (y + 0.5) * pixelSize

    // Note: camera looks toward z (i.e. +x is to the left)
    val worldX = halfWidth - xOffset
    val worldY = halfHeight - yOffset

    // transform the canvas point
    val pixel = transformation.inverse * Point(worldX, worldY, -1)
    // transform the origin
    val origin = transformation.inverse * Point.Origin

    val direction = (pixel - origin).normalize

    Ray(origin.toPoint, direction)
  }
}
