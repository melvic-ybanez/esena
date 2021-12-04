package com.melvic.esena.tuples

import com.melvic.esena
import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.Matrix
import com.melvic.esena.{MathUtils, Real}

import scala.language.implicitConversions

trait Tuple {
  val x: Real
  val y: Real
  val z: Real
  val w: Real

  def map(f: Real => Real): Tuple =
    Tuple(f(x), f(y), f(z), f(w))

  def +(that: Tuple): Tuple =
    Tuple(x + that.x, y + that.y, z + that.z, w + that.w)

  def -(that: Tuple): Tuple =
    Tuple(x - that.x, y - that.y, z - that.z, w - that.w)

  def unary_- : Tuple =
    Vec.Zero - this

  def *(scalar: Real): Tuple = map(_ * scalar)

  def /(scalar: Real): Tuple = this * (1 / scalar)

  lazy val magnitude: Real =
    math.sqrt(x * x + y * y + z * z + w * w)

  /**
    * Computes for the vector dot-product
    */
  def dot(that: Tuple): Real =
    x * that.x + y * that.y + z * that.z + w * that.w

  def  toPoint: Point = Point(x, y, z)

  def toVec: Vec = Vec(x, y, z)

  def toColor: Color = Color(x, y, z)

  override def equals(that: Any) = that match {
    case tuple: Tuple =>
      MathUtils.compareReals(this.x, tuple.x) &&
        esena.MathUtils.compareReals(this.y, tuple.y) &&
        esena.MathUtils.compareReals(this.z, tuple.z) &&
        esena.MathUtils.compareReals(this.w, tuple.w)
    case _ => false
  }

  override def toString = s"($x, $y, $z, $w)"
}

object Tuple {
  private case class TupleImpl(x: Real, y: Real, z: Real, w: Real) extends Tuple

  def apply(x: Real, y: Real, z: Real, w: Real): Tuple =
    TupleImpl(x, y, z, w)

  def isPoint(tuple: Tuple): Boolean = tuple.w == 1

  def isVector(tuple: Tuple): Boolean = tuple.w == 0

  def fromMatrix(matrix: Matrix): Tuple =
    Tuple(matrix(0, 0), matrix(1, 0), matrix(2, 0), matrix(3, 0))

  implicit def tupleToVec(tuple: Tuple): Vec =
    tuple.toVec

  implicit def tupleToPoint(tuple: Tuple): Point =
    tuple.toPoint

  implicit def toColor(tuple: Tuple): Color =
    tuple.toColor
}
