package com.melvic.esena.matrix

import com.melvic.esena.{Math, Tuple}
import com.melvic.esena.matrix.Matrix.{Elements, MatrixImpl}

trait Matrix {
  def width: Int
  def height: Int
  def elements: Elements

  def apply(row: Int, col: Int, element: Double): Matrix =
    MatrixImpl(width, height, elements.updated(Math.indexOf(row, col, width), element))

  def apply(row: Int, col: Int): Double =
    elements(Math.indexOf(row, col, width))

  def at(row: Int, col: Int): Double = this(row, col)

  def *(that: Matrix): Matrix = {
    val rows = for {
      row <- 0 until height
      col <- 0 until that.width
    } yield
      at(row, 0) * that(0, col) + at(row, 1) * that(1, col) +
        at(row, 2) * that(2, col) + at(row, 3) * that(3, col)

    MatrixImpl(that.width, height, rows.toVector)
  }

  def *(tuple: Tuple): Tuple = Tuple.fromMatrix(this * Matrix.fromTuple(tuple))

  lazy val transpose: Matrix =
    (0 until height).foldLeft(this) { (m, i) =>
      (0 until width).foldLeft(m) { (m, j) =>
        m(j, i, at(i, j))
      }
    }

  lazy val determinant: Double =
    if (width == 2 && height == 2) at(0, 0) * at(1, 1) - at(0, 1) * at(1, 0)
    else 0

  def subMatrix(row: Int, col: Int): Matrix = {
    val data = (0 until height).foldLeft(Vector.empty[Double]) { (es, i) =>
      if (i >= row && i < row + 1) es
      else (0 until width).foldLeft(es) { (es, j) =>
        if (j == col) es
        else es :+ at(i, j)
      }
    }

    Matrix.fromRows(data)
  }

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
    MatrixImpl(elements.head.size, elements.size, elements.flatten)

  def fromRows(row: Elements*): Matrix =
    fromTable(row.toVector)

  def fromTuple(tuple: Tuple): Matrix =
    fromRows(Vector(tuple.x), Vector(tuple.y), Vector(tuple.z), Vector(tuple.w))

  def identity(width: Int, height: Int): Matrix = {
    val m = ofSize(width, height)
    (0 until width).foldLeft(m)((m, i) => m(i, i, 1))
  }

  def identity4By4: Matrix = identity(4, 4)
}
