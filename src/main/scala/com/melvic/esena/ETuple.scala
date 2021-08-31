package com.melvic.esena

case class ETuple(x: Double, y: Double, z: Double, w: Double)

object ETuple {
  def isPoint(tuple: ETuple): Boolean = tuple.w == 1

  def isVector(tuple: ETuple): Boolean = tuple.w == 0
}
