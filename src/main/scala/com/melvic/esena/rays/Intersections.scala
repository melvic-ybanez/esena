package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape

object Intersections {
  type Intersections = Vector[Intersection]

  def apply(intersection: Intersection*): Intersections =
    Intersection.aggregate(intersection: _*)

  def apply(data: (Double, Shape)*)(implicit dummy: DummyImplicit): Intersections =
    Intersections.fromPairs(data: _*)

  def fromPairs(data: (Double, Shape)*): Intersections =
    Intersections(data.map { case (n, s) => Intersection(n, s) }: _*)

  def fromPairs(data: (Int, Shape)*)(implicit dummy: DummyImplicit): Intersections =
    Intersections(data.map { case (n, s) => Intersection(n.toDouble, s) }: _*)

  def schlick(comps: Computations): Double = {
    val cos = comps.eyeVec.dot(comps.normalVec)
    lazy val sin2T = {
      val n = comps.n1 / comps.n2
      n * n * (1.0 - cos * cos)
    }

    if (comps.n1 > comps.n2 && sin2T > 1.0) 1.0 else 0.0
  }
}
