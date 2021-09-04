package com.melvic.esena

final case class Canvas(width: Int, height: Int, pixels: Vector[Color]) {
  def writePixel(x: Int, y: Int, color: Color): Canvas =
    Canvas(width, height, pixels.updated(index(x, y), color))

  def pixelAt(x: Int, y: Int): Color =
    pixels(index(x, y))

  val ppm: String =
    s"P3\n$width $height\n255"

  private def index(x: Int, y: Int): Int =
    x * width + y
}

object Canvas {
  def apply(width: Int, height: Int): Canvas =
    Canvas(width, height, (0 until width * height).map(_ => Color(0, 0, 0)).toVector)
}
