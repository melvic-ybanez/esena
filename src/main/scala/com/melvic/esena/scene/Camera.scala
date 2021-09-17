package com.melvic.esena.scene

import com.melvic.esena.canvas.Canvas
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

  /**
    * Computes a ray that starts from the camera and passes
    * through the pixel on the canvas.
    * @param x the x coordinate of the pixel
    * @param y the y coordinate of the pixel
    */
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
    // compute the direction vector of the ray
    val direction = (pixel - origin).normalize

    Ray(origin.toPoint, direction)
  }

  def transform(transformation: Matrix): Camera =
    copy(transformation = transformation * this.transformation)

  /**
    * Render an image of the given world using this camera
    */
  def render(world: World, antialias: Boolean = true): Canvas =
    if (antialias) {
      // set to higher resolution first if antialiasing is considered
      val highRes = copy(hSize = hSize * 2, vSize = vSize * 2).render(world, antialias = false)

      val canvas = Canvas(hSize.toInt, vSize.toInt)

      // apply down-sampling to remove jagged edges
      (0 until canvas.height).foldLeft(canvas) { (canvas, y) =>
        val sourceY = y * 2
        (0 until canvas.width).foldLeft(canvas) { (canvas, x) =>
          val sourceX = x * 2
          val pixelAt = highRes.pixelAt _
          val average = (pixelAt(sourceX, sourceY) + pixelAt(sourceX, sourceY + 1) +
            pixelAt(sourceX + 1, sourceY) + pixelAt(sourceX + 1, sourceY + 1)) * 0.25
          canvas.writePixel(x, y, average)
        }
      }
    } else {
      val canvas = Canvas(hSize.toInt, vSize.toInt)
      (0 until canvas.height).foldLeft(canvas) { (canvas, y) =>
        (0 until canvas.width).foldLeft(canvas) { (canvas, x) =>
          val ray   = rayForPixel(x, y)
          val color = world.colorAt(ray)
          canvas.writePixel(x, y, color)
        }
      }
    }
}
