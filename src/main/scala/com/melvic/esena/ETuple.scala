package com.melvic.esena

final case class ETuple(x: Double, y: Double, z: Double, w: Double) {
  override def equals(that: Any) = that match {
    case ETuple(x, y, z, w) =>
      Utils.compareDoubles(this.x, x) &&
        Utils.compareDoubles(this.y, y) &&
        Utils.compareDoubles(this.z, z) &&
        Utils.compareDoubles(this.w, w)
    case _ => false
  }

  def +(that: ETuple): ETuple =
    ETuple(x + that.x, y + that.y, z + that.z, w + that.w)

  def -(that: ETuple): ETuple =
    ETuple(x - that.x, y - that.y, z - that.z, w - that.w)
}

object ETuple {
  def isPoint(tuple: ETuple): Boolean = tuple.w == 1

  def isVector(tuple: ETuple): Boolean = tuple.w == 0

  def point(x: Double, y: Double, z: Double): ETuple =
    ETuple(x, y, z, 1)

  def vector(x: Double, y: Double, z: Double): ETuple =
    ETuple(x, y, z, 0)

  def zeroVector: ETuple = vector(0, 0, 0)
}
