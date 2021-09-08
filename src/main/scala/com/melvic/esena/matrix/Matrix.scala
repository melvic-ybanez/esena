package com.melvic.esena.matrix

import com.melvic.esena.Math
import com.melvic.esena.matrix.Matrix.{Elements, MatrixImpl}
import com.melvic.esena.tuples.Tuple

trait Matrix {
  def width: Int
  def height: Int
  def elements: Elements

  /**
    * Updates the element at the specified position
    */
  def apply(row: Int, col: Int, element: Double): Matrix =
    MatrixImpl(width, height, elements.updated(Math.indexOf(row, col, width), element))

  /**
    * Gets the element at the specified position
    */
  def apply(row: Int, col: Int): Double =
    elements(Math.indexOf(row, col, width))

  def at(row: Int, col: Int): Double = this(row, col)

  /**
    * Matrix multiplication
    */
  def *(that: Matrix): Matrix = {
    val rows = for {
      row <- 0 until height
      col <- 0 until that.width
    } yield
      (0 until width).foldLeft(0.0) { (v, j) =>
        v + at(row, j) * that(j, col)
      }

    MatrixImpl(that.width, height, rows.toVector)
  }

  def *(tuple: Tuple): Tuple = Tuple.fromMatrix(this * Matrix.fromTuple(tuple))

  /**
    * Converts rows into columns and columns into rows.
    */
  lazy val transpose: Matrix =
    (0 until height).foldLeft(this) { (m, i) =>
      (0 until width).foldLeft(m) { (m, j) =>
        m(j, i, at(i, j))
      }
    }

  /**
    * Computes the determinant of the matrix. For 2x2, we use the
    * ac - bd formula. For dimensions above that, we use the more generic
    * formula A(0, 1) * cofactor(0, 1) + ... A(0, n -1) * cofactor(0, n - 1), for
    * any matrix A of width n. See [[cofactor]].
    */
  lazy val determinant: Double =
    if (width == 2 && height == 2) at(0, 0) * at(1, 1) - at(0, 1) * at(1, 0)
    else
      (0 until width).foldLeft(0.0) { (det, col) =>
        det + at(0, col) * cofactor(0, col)
      }

  /**
    * Removes the specified row and column of the matrix
    */
  def subMatrix(row: Int, col: Int): Matrix = {
    val data = (0 until height).foldLeft(Vector.empty[Double]) { (es, i) =>
      if (i >= row && i < row + 1) es
      else
        (0 until width).foldLeft(es) { (es, j) =>
          if (j == col) es
          else es :+ at(i, j)
        }
    }

    MatrixImpl(width - 1, height - 1, data)
  }

  /**
    * Computes the determinant of the sub-matrix
    */
  def minor(row: Int, col: Int): Double =
    subMatrix(row, col).determinant

  /**
    * Computes the determinant of the sub-matrix,
    * where every element might have its sign reversed.
    */
  def cofactor(row: Int, col: Int): Double =
    math.pow(-1, row + col) * subMatrix(row, col).determinant

  lazy val isInvertible: Boolean =
    determinant != 0

  lazy val inverse: Option[Matrix] =
    if (!isInvertible) None
    else Some {
      val data = for {
        row <- 0 until height
        col <- 0 until width
      } yield cofactor(row, col) / determinant

      MatrixImpl(width, height, data.toVector).transpose
    }

  override def equals(o: Any) = o match {
    case MatrixImpl(w, h, elements) =>
      val elementsEqual = elements
        .zip(this.elements)
        .forall { case (a, b) => Math.compareDoubles(a, b) }
      val sizeEqual = w == width && h == height
      elementsEqual && sizeEqual
    case _ => false
  }

  def map(f: Double => Double): Matrix =
    MatrixImpl(width, height, elements.map(f))

  override def toString: String = {
    def rowToString(elements: Elements): String =
      s"[${elements.mkString(",")}]"
    elements.grouped(width).map(rowToString).mkString("\n")
  }
}

object Matrix {
  type Elements = Vector[Double]
  type Vec4     = (Double, Double, Double, Double)
  type Vec3     = (Double, Double, Double)

  private case class MatrixImpl(width: Int, height: Int, elements: Elements) extends Matrix

  def ofSize(width: Int, height: Int): Matrix =
    MatrixImpl(width, height, Vector.fill(width * height)(0))

  def fromTable(elements: Vector[Elements]): Matrix =
    MatrixImpl(elements.head.size, elements.size, elements.flatten)

  def fromRows(row: Elements*): Matrix =
    fromTable(row.toVector)

  def fromTuple(tuple: Tuple): Matrix =
    fromRows(Vector(tuple.x), Vector(tuple.y), Vector(tuple.z), Vector(tuple.w))

  def of4By4(r1: Vec4, r2: Vec4, r3: Vec4, r4: Vec4): Matrix = {
    def toElems(tuple: Vec4): Elements = tuple match {
      case (x, y, z, w) => Vector(x, y, z, w)
    }
    fromRows(toElems(r1), toElems(r2), toElems(r3), toElems(r4))
  }

  def of3By3(r1: Vec3, r2: Vec3, r3: Vec3): Matrix = {
    def toElems(tuple: Vec3): Elements = tuple match {
      case (x, y, z) => Vector(x, y, z)
    }
    fromRows(toElems(r1), toElems(r2), toElems(r3))
  }

  def identity(width: Int, height: Int): Matrix = {
    val m = ofSize(width, height)
    (0 until width).foldLeft(m)((m, i) => m(i, i, 1))
  }

  def identity4By4: Matrix = identity(4, 4)
}
