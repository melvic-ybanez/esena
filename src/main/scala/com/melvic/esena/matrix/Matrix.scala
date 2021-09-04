package com.melvic.esena.matrix

import com.melvic.esena.Math
import com.melvic.esena.matrix.Matrix.{Elements, MatrixImpl}

trait Matrix {
  def width: Int
  def height: Int
  def elements: Elements

  def apply(row: Int, col: Int, element: Double): Matrix =
    MatrixImpl(width, height, elements.updated(Math.indexOf(row, col, width), element))

  def apply(row: Int, col: Int): Double =
    elements(Math.indexOf(row, col, width))

  override def equals(o: Any) = o match {
    case MatrixImpl(_, _, elements) =>
      elements.zip(this.elements).forall { case (a, b) => Math.compareDoubles(a, b) }
    case _ => false
  }
}

object Matrix {
  type Elements = Vector[Double]

  private case class MatrixImpl(width: Int, height: Int, elements: Elements) extends Matrix

  def ofSize(width: Int, height: Int): Matrix =
    MatrixImpl(width, height, Vector.fill(width * height)(0))

  def fromTable(elements: Vector[Elements]): Matrix =
    MatrixImpl(elements.size, elements.head.size, elements.flatten)

  def fromRows(row: Elements*): Matrix =
    fromTable(row.toVector)
}
