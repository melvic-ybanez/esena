package com.melvic.esena

import com.melvic.esena.Canvas.PixelSet

final case class Canvas(width: Int, height: Int, pixels: PixelSet, ppmId: String, maxColorValue: Int) {
  def writePixel(x: Int, y: Int, color: Color): Canvas =
    Canvas(width, height, pixels.updated(index(x, y), color), ppmId, maxColorValue)

  def pixelAt(x: Int, y: Int): Color =
    pixels(index(x, y))

  final case class Ppm(header: Vector[String], pixelData: Vector[String]) {
    override def toString =
      (header ++ pixelData).mkString("\n")
  }

  def ppm: Ppm = Ppm(
    Vector(ppmId, s"$width $height", maxColorValue.toString),
    pixels.map(_.ppmString(maxColorValue)).grouped(width).map(_.mkString(" ")).toVector
  )

  private def index(x: Int, y: Int): Int =
    y * width + x
}

object Canvas {
  type PixelSet = Vector[Color]

  val DefaultPpmId = "P3"
  val DefaultMaxColorValue = 255

  def apply(width: Int, height: Int): Canvas =
    Canvas(
      width,
      height,
      (0 until (width * height)).map(_ => Color(0, 0, 0)).toVector,
      DefaultPpmId,
      DefaultMaxColorValue
    )
}
