package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape

object Intersections {
  type Intersections = Vector[Intersection]

  def apply(intersection: Intersection*): Intersections =
    Intersection.aggregate(intersection: _*)

  def fromPairs(data: (Double, Shape)*): Intersections =
    Intersections(data.map { case (n, s) => Intersection(n, s) }: _*)
}
