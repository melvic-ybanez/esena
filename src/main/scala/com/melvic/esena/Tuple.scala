package com.melvic.esena

import com.melvic.esena
import com.melvic.esena.Tuple.TupleImpl
import com.melvic.esena.matrix.Matrix

trait Tuple {
  val x: Double
  val y: Double
  val z: Double
  val w: Double

  def map(f: Double => Double): Tuple =
    Tuple(f(x), f(y), f(z), f(w))

  def +(that: Tuple): Tuple =
    Tuple(x + that.x, y + that.y, z + that.z, w + that.w)

  def -(that: Tuple): Tuple =
    Tuple(x - that.x, y - that.y, z - that.z, w - that.w)

  def unary_- : Tuple =
    Vec.Zero - this

  def *(scalar: Scalar): Tuple = map(_ * scalar)

  def /(scalar: Scalar): Tuple = this * (1 / scalar)

  lazy val magnitude: Double =
    math.sqrt(x * x + y * y + z * z + w * w)

  /**
    * Computes for the vector dot-product
    */
  def dot(that: Tuple): Scalar =
    x * that.x + y * that.y + z * that.z + w * that.w

  override def equals(that: Any) = that match {
    case tuple: Tuple =>
      Math.compareDoubles(this.x, tuple.x) &&
        esena.Math.compareDoubles(this.y, tuple.y) &&
        esena.Math.compareDoubles(this.z, tuple.z) &&
        esena.Math.compareDoubles(this.w, tuple.w)
    case _ => false
  }

  override def toString = s"($x, $y, $z, $w)"
}

object Tuple {
  private case class TupleImpl(x: Double, y: Double, z: Double, w: Double) extends Tuple

  def apply(x: Double, y: Double, z: Double, w: Double): Tuple =
    TupleImpl(x, y, z, w)

  def isPoint(tuple: Tuple): Boolean = tuple.w == 1

  def isVector(tuple: Tuple): Boolean = tuple.w == 0

  def fromMatrix(matrix: Matrix): Tuple =
    Tuple(matrix(0, 0), matrix(1, 0), matrix(2, 0), matrix(3, 0))
}
