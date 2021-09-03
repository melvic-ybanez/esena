package com.melvic.esena

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
    Vec.zero - this

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
    case Tuple(x, y, z, w) =>
      Utils.compareDoubles(this.x, x) &&
        Utils.compareDoubles(this.y, y) &&
        Utils.compareDoubles(this.z, z) &&
        Utils.compareDoubles(this.w, w)
    case _ => false
  }

  override def toString = s"($x, $y, $z, $w)"
}

object Tuple {
  def apply(_x: Double, _y: Double, _z: Double, _w: Double): Tuple =
    new Tuple {
      override val x = _x
      override val y = _y
      override val z = _z
      override val w = _w
    }

  def unapply(tuple: Tuple): Option[(Double, Double, Double, Double)] =
    Some(tuple.x, tuple.y, tuple.z, tuple.w)

  def isPoint(tuple: Tuple): Boolean = tuple.w == 1

  def isVector(tuple: Tuple): Boolean = tuple.w == 0
}
