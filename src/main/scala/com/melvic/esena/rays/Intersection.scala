package com.melvic.esena.rays

final case class Intersection[S](t: Double, obj: S)

object Intersection {
  def aggregate[S](intersection: Intersection[S]*): Vector[Intersection[S]] =
    intersection.toVector
}
