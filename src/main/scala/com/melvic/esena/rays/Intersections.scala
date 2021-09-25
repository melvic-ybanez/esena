package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape

object Intersections {
  type Intersections = Vector[Intersection]

  val None = Vector.empty[Intersection]

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

    def reflectance(cos: Double) = {
      val r = math.pow((comps.n1 - comps.n2) / (comps.n1 + comps.n2), 2)
      r + (1 - r) * math.pow(1 - cos, 5)
    }

    // total internal reflection only happens if n1 > n2
    if (comps.n1 > comps.n2) {
      val n = comps.n1 / comps.n2
      val sin2T = n * n * (1.0 - cos * cos)

      if (sin2T > 1.0) 1.0
      else reflectance(math.sqrt(1.0 - sin2T))
    } else reflectance(cos)
  }

  def maybeOne(cond: Boolean, pair: (Double, Shape)): Intersections =
    if (cond) Intersections(pair)
    else Intersections.None
}
