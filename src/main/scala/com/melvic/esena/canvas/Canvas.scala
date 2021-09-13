package com.melvic.esena.canvas

import com.melvic.esena.MathUtils
import com.melvic.esena.canvas.Canvas.{CanvasImpl, PixelSet}

import scala.annotation.tailrec

trait Canvas {
  def width: Int
  def height: Int
  def pixels: PixelSet
  def ppmId: String
  def maxColorValue: Int

  def writePixel(x: Int, y: Int, color: Color): Canvas =
    writePixelAtIndex(index(x, y), color)

  def writePixelAtIndex(pos: Int, color: Color): Canvas =
    CanvasImpl(width, height, pixels.updated(pos, color), ppmId, maxColorValue)

  def pixelAt(x: Int, y: Int): Color =
    pixels(index(x, y))

  final case class Ppm(header: Vector[String], pixelData: Vector[String]) {
    override def toString =
      // the extra newline is required by other image programs
      (header ++ pixelData).mkString("\n") + "\n"
  }

  def ppm: Ppm = {

    /**
      * Checks if the number of characters in a line do not exceed 70.
      * If it does, move the excess to the next line.
      */
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
    MathUtils.indexOf(y, x, width)
}

object Canvas {
  type PixelSet = Vector[Color]

  val DefaultPpmId         = "P3"
  val DefaultMaxColorValue = 255

  private case class CanvasImpl(
      width: Int,
      height: Int,
      pixels: PixelSet,
      ppmId: String,
      maxColorValue: Int
  ) extends Canvas

  def apply(width: Int, height: Int): Canvas =
    CanvasImpl(
      width,
      height,
      Vector.fill(width * height)(Color(0, 0, 0)),
      DefaultPpmId,
      DefaultMaxColorValue
    )
}
