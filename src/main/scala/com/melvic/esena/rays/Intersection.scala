package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape

final case class Intersection(t: Double, obj: Shape)

object Intersection {
  def aggregate(intersection: Intersection*): Vector[Intersection] =
    intersection.toVector
}
