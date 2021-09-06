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
    } yield (0 until width).foldLeft(0.0) { (v, j) =>
      v + at(row, j) * that(j, col)
    }

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
    else (0 until width).foldLeft(0.0) { (det, col) =>
      det + at(0, col) * cofactor(0, col)
    }

  def subMatrixWith(row: Int, col: Int)(f: (Double, Int, Int) => Double): Matrix = {
    val data = (0 until height).foldLeft(Vector.empty[Double]) { (es, i) =>
      if (i >= row && i < row + 1) es
      else (0 until width).foldLeft(es) { (es, j) =>
        if (j == col) es
        else es :+ f(at(i, j), i, j)
      }
    }

    MatrixImpl(width - 1, height - 1, data)
  }

  def subMatrix(row: Int, col: Int): Matrix =
    subMatrixWith(row, col)((elem, _, _) => elem)

  def minor(row: Int, col: Int): Double =
    subMatrix(row, col).determinant

  def cofactor(row: Int, col: Int): Double = {
    val minorWithAlteredSigns = subMatrixWith(row, col) { (elem, i, j) =>
      if (Math.indexOf(i, j, width) % 2 == 0) elem else -elem
    }
    minorWithAlteredSigns.determinant
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
}

object Matrix {
  type Elements = Vector[Double]
  type Vec4 = (Double, Double, Double, Double)
  type Vec3 = (Double, Double, Double)

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
