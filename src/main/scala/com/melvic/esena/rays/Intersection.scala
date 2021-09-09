package com.melvic.esena.rays

import com.melvic.esena.shapes.Shape

final case class Intersection(t: Double, obj: Shape)

object Intersection {
  type Intersections = Vector[Intersection]

  def aggregate(intersection: Intersection*): Intersections =
    intersection.toVector

  def hit(intersections: Intersections): Option[Intersection] = {
    val xs = intersections.filter(_.t > 0)
    if (xs.isEmpty) None
    else Some(xs.minBy(_.t))
  }

  /**
   * Unsafe form of [[hit]] which assumes that a hit does exist.
   */
  def getHit(intersections: Intersections): Intersection =
    hit(intersections).get
}
