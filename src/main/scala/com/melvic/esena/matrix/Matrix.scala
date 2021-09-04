package com.melvic.esena.matrix

import com.melvic.esena.Math
import com.melvic.esena.matrix.Matrix.Elements

final case class Matrix(width: Int, height: Int, elements: Elements) {
  def apply(row: Int, col: Int, element: Double): Matrix =
    Matrix(width, height, elements.updated(Math.indexOf(row, col, width), element))

  def apply(row: Int, col: Int): Double =
    elements(Math.indexOf(row, col, width))
}

object Matrix {
  type Elements = Vector[Double]

  def ofSize(width: Int, height: Int): Matrix =
    Matrix(width, height, Vector.fill(width * height)(0))

  def fromTable(elements: Vector[Elements]): Matrix =
    apply(elements.size, elements.head.size, elements.flatten)

  def fromRows(row: Elements*): Matrix =
    fromTable(row.toVector)
}
