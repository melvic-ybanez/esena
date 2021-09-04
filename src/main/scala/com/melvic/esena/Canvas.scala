package com.melvic.esena

import com.melvic.esena.Canvas.PixelSet

import scala.annotation.tailrec

final case class Canvas(width: Int, height: Int, pixels: PixelSet, ppmId: String, maxColorValue: Int) {
  def writePixel(x: Int, y: Int, color: Color): Canvas =
    writePixelAtIndex(index(x, y), color)

  def writePixelAtIndex(pos: Int, color: Color): Canvas =
    Canvas(width, height, pixels.updated(pos, color), ppmId, maxColorValue)

  def pixelAt(x: Int, y: Int): Color =
    pixels(index(x, y))

  final case class Ppm(header: Vector[String], pixelData: Vector[String]) {
    override def toString =
      // the extra newline is required by other image programs
      (header ++ pixelData).mkString("\n") + "\n"
  }

  def ppm: Ppm = {
    def wrap(line: String): Vector[String] = {
      @tailrec
      def recurse(acc: Vector[String], xs: Vector[String]): Vector[String] =
        (acc, xs) match {
          case (_, IndexedSeq()) => acc
          case (IndexedSeq(), next +: nextRest) =>
            recurse(Vector(next), nextRest)
          case (prev +: prevRest, next +: nextRest) =>
            if (prev.length + next.length > 69) recurse(next +: acc, nextRest)
            else recurse((prev + " " + next) +: prevRest, nextRest)
        }
      recurse(Vector.empty, line.split(" ").toVector).reverse
    }

    Ppm(
      Vector(ppmId, s"$width $height", maxColorValue.toString),
      pixels
        .map(_.ppmString(maxColorValue))
        .grouped(width)
        .toVector
        .flatMap(v => wrap(v.mkString(" ")))
    )
  }

  private def index(x: Int, y: Int): Int =
    y * width + x
}

object Canvas {
  type PixelSet = Vector[Color]

  val DefaultPpmId         = "P3"
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
