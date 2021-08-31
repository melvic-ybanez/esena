package com.melvic.esena

case class ETuple(x: Double, y: Double, z: Double, w: Double)

object ETuple {
  def isPoint(tuple: ETuple): Boolean = tuple.w == 1

  def isVector(tuple: ETuple): Boolean = tuple.w == 0

  def point(x: Double, y: Double, z: Double): ETuple =
    ETuple(x, y, z, 1)

  def vector(x: Double, y: Double, z: Double): ETuple =
    ETuple(x, y, z, 0)
}
